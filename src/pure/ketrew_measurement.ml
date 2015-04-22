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

open Ketrew_pervasives

module Measurement_collection = struct
  type http_request = {
    connection_id: string;
    meth: string;
    uri: string;
  } [@@deriving yojson]
  type response_log = {
    response: string;
    body_length: int;
  } [@@deriving yojson]
  type measurement = [
    | `Creation
    | `Incoming_request of http_request
    | `End_of_request of (http_request * response_log)
    | `Tag of string
  ] [@@deriving yojson]
  type measurement_item = {
    time: Time.t;
    content: measurement;
  } [@@deriving yojson]
  type t = measurement_item list [@@deriving yojson]
end
include Json.Versioned.Of_v0(Measurement_collection)
include Measurement_collection

module Item = struct
  type t = measurement_item
  let create content = 
    { time = Time.(now ()); content}

  let make_http_request connection_id request =
    let meth = Cohttp.Request.meth request |> Cohttp.Code.string_of_method in
    let uri = Cohttp.Request.uri request |> Uri.to_string in
    {connection_id;  meth; uri}

  let make_reponse_log response body_length =
    {response; body_length}

  let incoming_request r = create (`Incoming_request r)
  let end_of_request r rl = create (`End_of_request (r, rl))

  let tag s = create (`Tag s)
                           
  let time i = i.time
  let compare_by_time ia ib =
    Float.compare ia.time ib.time

  let to_strings item =
    let date = Time.to_filename (time item) in
    match item.content with
    | `Creation -> [date; "Creation"]
    | `Incoming_request hr -> [date; "Incomming HTTP request"; hr.uri]
    | `End_of_request (hr, rl) ->
      [date; "End of HTTP request";
       hr.uri;
       Int.to_string rl.body_length;
       rl.response; ]
    | `Tag t -> [date; "Tag"; t]

  let collect_http_requests item_list =
    let r = ref [] in
    List.iter item_list ~f:(fun item ->
        let date = item.time in
        match item.content with
        | `Tag _ | `Creation -> ()
        | `Incoming_request hr ->
          r := (hr, date, None) :: !r
        | `End_of_request (hr, rl) ->
          r := List.map !r ~f:(function
            | (h, i, None) when h = hr -> (h, i, Some (date, rl))
            | other -> other)
      );
    List.filter_map !r ~f:(function
      | (hr, t, Some (t2, rl)) ->
        Some (object
          method uri = hr.uri
          method date = t
          method duration = (t2 -. t)
          method body_length = rl.body_length
        end)
      | _ -> None)

end

module Collection = struct
  type t = Measurement_collection.t ref
  let serialize t = serialize !t
  let deserialize_exn t = ref (deserialize_exn t)

  let create () = ref [Item.create `Creation]
  let add collection log = 
    collection := log :: !collection
  let clear c = c := [Item.create `Creation]

  let concat cl : t =
    List.concat_map cl ~f:(fun c -> !c) |> ref

  let to_list c : Item.t list = !c

end

