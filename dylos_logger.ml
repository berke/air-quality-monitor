(* Dylos_logger *)

(**

   Tool for logging the output of Dylos air quality monitors

   Copyright (C) 2014-2016 Berke Durak

   Author: Berke Durak <berke.durak@gmail.com>

 *)

open Lwt
open Lwt_log
open Printf

module Opt =
  struct
    let port = ref "/dev/ttyUSB0"
    let baud = ref 9600
    let log_file = ref "dylos.log"
    let session_file : string option ref = ref None
    let heartbeat_delay = ref 600.0
  end

module Spec =
  struct
    open Arg
    open Opt

    let specs =
      align @@
	[
	  "--port",
	  Set_string port,
	  "<path> Set serial port";

	  "--baud",
	  Set_int baud,
	  "<rate> Set port speed";

	  "--log-file",
	  Set_string log_file,
	  "<path> Set log file";

	  "--session-file",
	  String(fun u -> session_file := Some u),
	  "<path> Use session file to provide a persistent serial number";

	  "--heartbeat-delay",
	  Set_float heartbeat_delay,
	  "<float> Interval between heartbeats";
	]
  end

let line_splitter ?(limit=128) ic =
  let b = Buffer.create 128 in
  let rec loop () =
    Lwt_io.read_char ic >>= fun c ->
    match c with
    | '\n' -> loop ()
    | '\r' ->
       let u = Buffer.contents b in
       Buffer.clear b;
       debug_f "<< %S" u >>= fun () ->
       return u
    | c when Buffer.length b < limit ->
       Buffer.add_char b c;
       loop ()
    | _ ->
       loop ()
  in
  loop

exception Remote_error of string

let set_baud baud tio =
  let open Unix in
  {
    tio with
    c_obaud = baud;
    c_ibaud = baud;
  }

let make_raw tio =
  let open Unix in
  {
    tio with
    c_istrip = false;
    c_inlcr = false;
    c_igncr = false;
    c_icrnl = false;
    c_ixoff = false;
    c_ixon = false;
    c_csize = 8;
    c_cstopb = 1;
    c_cread = true;
    c_parenb = false;
    c_hupcl = false;
    c_clocal = true;
    c_isig = false;
    c_icanon = false;
    c_noflsh = true;
    c_echo = false;
    c_echoe = false;
    c_echok = false;
    c_echonl = false;
    c_vmin = 1;
    c_vtime = 0;
  } 

let serial_setup port baud =
  Lwt_unix.openfile port [Unix.O_RDWR;Unix.O_NONBLOCK] 0o644 >>= fun fd ->
  Lwt_unix.tcgetattr fd >>= fun ta ->
  let ta' = set_baud baud @@ make_raw ta in
  Lwt_unix.tcsetattr fd Unix.TCSANOW ta' >>= fun _ ->
  let r_ch = Lwt_io.of_fd ~mode:Lwt_io.input fd
  and w_ch = Lwt_io.of_fd ~mode:Lwt_io.output fd
  in
  return (r_ch, w_ch)

let main () =
  let open Opt in

  serial_setup !port !baud >>= fun (serial_r_ch, serial_w_ch) ->

  let drain timeout =
    let drainer =
      let rec loop () = Lwt_io.read_char serial_r_ch >>= fun c -> loop () in
      loop ()
    in
    let sleeper =
      Lwt_unix.sleep timeout >>= fun () ->
      cancel drainer;
      return ()
    in
    try_bind
      (fun () -> join [sleeper; drainer])
      (fun () -> return ())
      (function
	| Canceled -> return ()
	| e -> fail e)
  in

  drain 1.0 >>= fun () ->

  let next_line = line_splitter serial_r_ch in

  let t0 = Unix.gettimeofday () in
  let counter = ref 0 in

  (match !Opt.session_file with
   | None -> return 0
   | Some fn ->
      (catch
	 (fun () ->
	  Lwt_unix.stat fn >>= fun st ->
	  if st.Unix.st_kind = Unix.S_REG then
	    Lwt_io.with_file
	      ~perm:0o755
	      ~mode:Lwt_io.input
	      fn
	      (fun ic ->
	       Lwt_io.read_line ic >>= fun u ->
	       return @@ int_of_string u) 
	  else
	    (
	      info_f "Session file %S is not regular" fn >>= fun () ->
	      return 0
	    )
	 )
	 (fun e ->
	  error_f "Cannot read from session file %S: %s"
		  fn
		  (Printexc.to_string e) >>= fun () ->
	  return 0)) >>= fun session ->
      let session' = session + 1 in
      Lwt_io.with_file
	~flags:Unix.([O_DSYNC;O_TRUNC;O_CREAT;O_WRONLY])
	~perm:0o755
	~mode:Lwt_io.output
	fn
	(fun oc -> Lwt_io.fprintf oc "%d" session') >>= fun () ->
      return session
  ) >>= fun session ->
  
  let rec heartbeat_task () =
    let dt = Unix.gettimeofday () -. t0 in
    (
      if dt > 0.0 then
	info_f "Session %d: %d readings in %f s (%f reading / s)"
	       session
	       !counter
	       dt
	       (float !counter /. dt)
      else
	info_f "Starting"
    ) >>= fun () ->
    Lwt_unix.sleep !heartbeat_delay >>=
      heartbeat_task
  in

  let main oc =
    let rec loop () =
      next_line () >>= fun u ->
      (
	match
	  (

	    try
	      Scanf.sscanf u "%d,%d" (fun x y -> Some(x,y))
	    with
	    | _ -> None
	  )
	with
	| None -> error_f "Cannot parse %S" u
	| Some(x,y) ->
	   incr counter;
	   let open Unix in
	   let t = gettimeofday () in
	   let tm = localtime t in
	   Lwt_io.fprintf oc
			  "%5d %8d %04d-%02d-%02d %02d:%02d:%02d %16.4f %5d %5d\n"
			  session !counter
			  (tm.tm_year + 1900)
			  (tm.tm_mon + 1)
			  tm.tm_mday
			  tm.tm_hour
			  tm.tm_min
			  tm.tm_sec
			  t
			  x
			  y
	   >>= fun () ->
	   Lwt_io.flush oc
      )
      >>= loop
    in
    loop ()
  in

  heartbeat_task () <&>
    Lwt_io.with_file
      ~flags:Unix.([O_DSYNC;O_APPEND;O_CREAT;O_WRONLY])
      ~perm:0o755
      ~mode:Lwt_io.output
      !Opt.log_file
      main

let _ =
  (
    try
      Arg.parse
        Spec.specs
        (fun _ -> raise @@ Arg.Bad "No extra arguments allowed")
        (Printf.sprintf "Usage: %s [options]" Sys.argv.(0));
    with
    | Arg.Bad b ->
       Printf.fprintf Pervasives.stderr "%s\n%s\n"
		      b
		      (Arg.usage_string Spec.specs "Usage:")
  );
  Lwt_main.run @@ main ()
