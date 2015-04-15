(**************************************************************************)
(*  Copyright 2015, Sebastien Mondet <seb@mondet.org>                     *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)

include  Pvem_lwt_unix
include  Pvem_lwt_unix.Deferred_result
let wrap_preemptively ~on_exn f =
  wrap_deferred (fun () -> 
      Lwt_preemptive.detach f ())
    ~on_exn

let lwt_stream_to_string lwt_stream =
  let buf = Buffer.create 42 in
  wrap_deferred ~on_exn:(fun e -> `Failure (Printexc.to_string e))
    Lwt.(fun () ->
        Lwt_stream.iter_s 
          (fun s -> Buffer.add_string buf s; return ()) lwt_stream)
  >>= fun () ->
  return (Buffer.contents buf)
