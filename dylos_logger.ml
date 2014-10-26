(* Dylos_logger *)

(**

   Tool for logging the output of Dylos air quality monitors

   Copyright (C) 2014 Berke Durak

   Author: Berke Durak <berke.durak@gmail.com>

*)

open Lwt
open Lwt_react
open Lwt_log
open Printf

module Opt =
struct
  let port = ref "/dev/ttyUSB0"
  let baud = ref 9600
  let log_file = ref "dylos.log"
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
    ]
end

let line_splitter ?(limit=128) ic =
  let b = Buffer.create 128 in
  let rec loop () =
    lwt c = Lwt_io.read_char ic in
    match c with
    | '\r' ->
      let u = Buffer.contents b in
      Buffer.clear b;
      debug_f "<< %S" u >>
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
  lwt fd = Lwt_unix.openfile port [Unix.O_RDWR;Unix.O_NONBLOCK] 0o644 in
  lwt ta = Lwt_unix.tcgetattr fd in
  let ta' = set_baud baud @@ make_raw ta in
  lwt _ = Lwt_unix.tcsetattr fd Unix.TCSANOW ta' in
  let r_ch = Lwt_io.of_fd ~mode:Lwt_io.input fd
  and w_ch = Lwt_io.of_fd ~mode:Lwt_io.output fd
  in
  return (r_ch, w_ch)

let main () =
  let open Opt in

  lwt serial_r_ch, serial_w_ch = serial_setup !port !baud in

  let drain timeout =
    let drainer =
      while_lwt true do
        lwt _ = Lwt_io.read_char serial_r_ch in
        return ()
      done
    in
    let sleeper =
      lwt () = Lwt_unix.sleep timeout in
      cancel drainer;
      return ()
    in
    try_lwt
      join [sleeper; drainer]
    with
    | Canceled -> return ()
  in

  lwt () = drain 1.0 in

  let next_line = line_splitter serial_r_ch in

  let main oc =
    let rec loop () =
      lwt u = next_line () in
      info_f "<< %S" u >>
      (
        match
          try
            Scanf.scanf "%d,%d" (fun x y -> Some(x,y))
          with
          | _ -> None
          with
          | None -> error_f "Cannot parse %S" u
          | Some(x,y) ->
            let t = Unix.gettimeofday () in
            Lwt_io.fprintf oc "%.3f %d %d\n" t x y >>
            Lwt_io.flush oc
      )
      >>
      loop ()
    in
    loop ()
  in

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

(* vim:set tw=80 cc=80 sw=2 ts=2 expandtab: *)
