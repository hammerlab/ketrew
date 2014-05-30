(** Manage external processes. *)

open Ketrew_pervasives

module Exit_code = struct
  type t = [
    | `Exited of int
    | `Signaled of int
    | `Stopped of int
  ]
  let to_string = function
  | `Exited n ->   fmt "exited:%d" n
  | `Signaled n -> fmt "signaled:%d" n
  | `Stopped n ->  fmt "stopped:%d" n

end

let exec ?(bin="") argl =
  wrap_deferred ~on_exn:(fun e -> `Process (`Exec (bin, argl), `Exn e))
    Lwt.(fun () ->
        let command = (bin, Array.of_list argl) in
        let process = Lwt_process.open_process_full command in
        Lwt_list.map_p Lwt_io.read
          [process#stdout; process#stderr; ]
        >>= fun output_2list ->
        process#status >>= fun status ->
        return (status, output_2list))
  >>= fun (ret, output_2list) ->
  let code =
    match ret with
    | Lwt_unix.WEXITED n ->   (`Exited n)
    | Lwt_unix.WSIGNALED n -> (`Signaled n)
    | Lwt_unix.WSTOPPED n ->  (`Stopped n)
  in
  begin match output_2list with
  | [out; err] -> return (out, err, code)
  | _ -> assert false
  end

let succeed ?(bin="") argl =
  exec ~bin argl
  >>= fun (out, err, status) ->
  let failure fmt =
    ksprintf (fun s -> fail (`Process (`Exec (bin, argl), `Non_zero s)))
      fmt in
  begin match status with
  | `Exited 0 -> return  (out, err)
  | code -> failure "%s" (Exit_code.to_string code)
  end

let error_to_string = function
| `Process (`Exec (bin, cmd), how) ->
  fmt "Executing %S[%s]: %s"
    bin (String.concat ~sep:", " (List.map cmd ~f:(fmt "%S")))
    (match how with
     | `Exn e -> fmt "Exn %s" @@ Printexc.to_string e
     | `Non_zero s -> fmt "Non-zero: %s" s)
