(**************************************************************************)
(*  Copyright 2014, Sebastien Mondet <seb@mondet.org>                     *)
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

open Ketrew_pervasives

module Down_message : sig
  type t = Ketrew_gen_protocol_v0.Down_message.t

  val to_json : Ketrew_gen_protocol_v0.Down_message.t -> CConvYojson.t
  val of_json_exn :
    Ketrew_pervasives.Json.t -> Ketrew_gen_protocol_v0.Down_message.t

  val serialize : Ketrew_gen_protocol_v0.Down_message.t -> string
  val deserialize_exn : string -> Ketrew_gen_protocol_v0.Down_message.t

  val log : t -> Ketrew_pervasives.Log.t
end

module Post_message : sig
  type t = Ketrew_gen_protocol_v0.Post_message.t

  val to_json : Ketrew_gen_protocol_v0.Post_message.t -> CConvYojson.t
  val of_json_exn :
    Ketrew_pervasives.Json.t -> Ketrew_gen_protocol_v0.Post_message.t

  val serialize : Ketrew_gen_protocol_v0.Post_message.t -> string
  val deserialize_exn : string -> Ketrew_gen_protocol_v0.Post_message.t

  val log : t -> Ketrew_pervasives.Log.t
  val to_string_hum : t -> string
end
