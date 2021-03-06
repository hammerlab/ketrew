(**************************************************************************)
(*    Copyright 2014, 2015:                                               *)
(*          Sebastien Mondet <seb@mondet.org>,                            *)
(*          Leonid Rozenberg <leonidr@gmail.com>,                         *)
(*          Arun Ahuja <aahuja11@gmail.com>,                              *)
(*          Jeff Hammerbacher <jeff.hammerbacher@gmail.com>               *)
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

open Ketrew_pure
open Internal_pervasives
open Unix_io

let initialized = ref false
let init () =
  if not !initialized then (
    let open Unix in
    let term_init = tcgetattr stdin in
    at_exit (fun () -> tcsetattr stdin TCSADRAIN term_init);
    initialized := true;
    ()
  )

let verbose_ref = ref false

let pressed key =
  if !verbose_ref then
    Log.(sf "%c pressed" key @ normal)

let toggle_verbose () =
  begin
    verbose_ref := not !verbose_ref;
    pressed 'v'
  end

(** [with_cbreak f] calls with the terminal in “get key” mode.
    It comes from http://pleac.sourceforge.net/pleac_ocaml/userinterfaces.html
*)
let with_cbreak (f: unit -> (_, _) t) =
  init ();
  let open Lwt_unix in
  Lwt.(tcgetattr stdin
        >>= fun term_init ->
        let go_back_to_normal () = tcsetattr stdin TCSADRAIN term_init in
        let term_cbreak = { term_init with c_icanon = false; c_echo = false } in
        tcsetattr stdin TCSANOW term_cbreak
        >>= fun () ->
        catch f (fun e ->
            Log.(s "with_cbreak exn: " % exn e @ warning);
            return (`Error (`Failure "with_cbreak")))
        >>= fun res ->
        go_back_to_normal ()
        >>= fun () ->
        return res)

let get_key () =
  match Configuration.use_cbreak () with
  | true ->
    with_cbreak (fun () ->
        wrap_deferred (fun () -> Lwt_io.read_char Lwt_io.stdin)
          ~on_exn:(fun e -> (`Failure "get_key")))
  | false ->
    wrap_deferred
      ~on_exn:(fun e -> (`Failure "get_key"))
      begin fun () ->
        let open Lwt in
        Lwt_io.read_line Lwt_io.stdin
        >>= fun l ->
        begin match String.get l 0 with
        | Some c -> return c
        | None -> return '\n'
        end
      end

let get_key_question () =
  match Configuration.use_cbreak () with
  | true -> "Press a single key:"
  | false -> "Enter a character and <enter>:"

let quit_key_information () =
  match Configuration.use_cbreak () with
  | true -> "Press the 'q' key to stop."
  | false -> "Use the 'q' and then <return> to stop."

(* cf. http://pleac.sourceforge.net/pleac_ocaml/userinterfaces.html *)
let read_password_exn () =
  let term_init = Unix.tcgetattr Unix.stdin in
  let term_no_echo = { term_init with Unix.c_echo = false } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW term_no_echo;
  try
    let password = read_line () in
    print_newline ();
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH term_init;
    password
  with e ->
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH term_init;
    raise e

let open_in_dollar_editor file =
  let editor =
    try Sys.getenv "EDITOR"
    with _ ->
      Log.(s "Using `vi` since $EDITOR is not defined" @ warning);
      "vi" in
  let command = fmt "%s %s" editor file in
  Log.(s "Running " % s command @ verbose);
  (* We actually want (for now) to block the whole process and wait for
      the editor to end. *)
  ignore (Sys.command command);
  return ()

let view_in_dollar_editor ?(extension="txt") content =
  let tmp =
    Filename.concat (Filename.get_temp_dir_name ())
      (fmt "%s.%s" (Unique_id.create ()) extension) in
  IO.write_file ~content tmp
  >>= fun () ->
  open_in_dollar_editor tmp

let ask_for_edition ?(extension=".conf") content =
  let tmpfile = Filename.temp_file "ketrew" extension in
  IO.write_file tmpfile ~content
  >>= fun () ->
  open_in_dollar_editor tmpfile
  >>= fun () ->
  IO.read_file tmpfile

let interaction_chars =
  List.init 10 (fun i -> Char.chr (48 + i))
  @ List.init 26 (fun i -> Char.chr (97 + i))

type 'a menu_item = (char option * Log.t * 'a)

let menu_item ?char ?(log=Log.empty) v =
  (char, log, v)

let menu ?(max_per_page=15) ?(always_there=[]) ~sentence items =
  (* let item_number = List.length items in *)
  let items_splitted =
    let split_number = max_per_page - (List.length always_there) in
    let rec split acc l =
      let left, right = List.split_n l split_number in
      match right with
      | [] -> List.rev (left :: acc)
      | more -> split (left :: acc) more
    in
    split [] items
  in
  let number_of_menus = List.length items_splitted in
  let rec menu_loop nth =
    let items =
      always_there @ (List.nth items_splitted nth |> Option.value ~default:[])
    in
    let available_chars =
      List.filter interaction_chars (fun c ->
          List.for_all items (fun (k, _, _) -> k <> Some c)) in
    let filled_items =
      let n = ref 0 in
      List.map items ~f:(function
        | None, l, v -> (incr n; List.nth available_chars !n, l, v)
        | Some k, l, v -> Some k, l, v)
    in
    let can_previous, can_next =
      match number_of_menus, nth with
      | 1, 0 -> false, false
      | _, 0 -> false, true
      | n, t when t = n - 1 -> true, false
      | _, _ -> true, true
    in
    Log.(sentence % sp
         % (if nth = 0 && number_of_menus = 1
            then empty
            else brakets (i (nth + 1) % s "/" % i number_of_menus)) % n
         % s (get_key_question ()) % n
         % concat
           (List.map filled_items ~f:(function
              | (Some k, l, v) -> sf "* [%c]: " k % l % n
              | None, _, _ -> s "ERROR, wrong usage of `menu`"))
         % (if can_previous then sf "* [<]: previous screen" % n else empty)
         % (if can_next then sf "* [>]: next screen" % n else empty)
         @ normal);
    get_key ()
    >>= fun key ->
    pressed key;
    begin match key with
    | '<' when can_previous -> menu_loop (nth - 1)
    | '>' when can_next -> menu_loop (nth + 1)
    | other ->
      begin match List.find filled_items (fun (k, _, _) -> k = Some key) with
      | Some (_, _, v) -> return v
      | None ->
        Log.(sf "Cannot understand: %c, try again please." key @ normal);
        menu_loop nth
      end
    end
  in
  menu_loop 0


(** Sort a list of targets from the most recent to the oldest. *)
let sort_target_list =
  List.sort ~cmp:(fun ta tb ->
      let f t = Ketrew_pure.Target.(state t |> State.passive_time) in
      compare (f tb) (f ta))

let build_sublist_of_targets ~client ~list_name ~all_log ~go_verb ~filter =
  Client.get_list_of_target_ids client
    ~query:Protocol.Up_message.{time_constraint = `All; filter}
  >>= fun id_list ->
  begin match id_list with
  | [] -> return []
  | more ->
    Client.get_targets client ~id_list
    >>| sort_target_list
  end
  >>= fun all_targets ->
  let to_process = ref [] in
  let all_valid_targets () =
    List.filter all_targets ~f:(fun target ->
        not (List.exists !to_process ~f:(fun id -> id = Target.id target)))
  in
  let target_menu () =
    List.map (all_valid_targets ()) ~f:(fun t ->
        menu_item
          ~log:Log.(s "Add: " % Document.target_for_menu t)
          (`Add (Target.id t)))
  in
  let rec loop () =
    let all_valid_ids = all_valid_targets () |> List.map ~f:Target.id in
    let always_there =
      let go =
        if !to_process = [] then []
        else
          let log = Log.(s "Go; " % if_color bold_red go_verb % s " the "
                         % (match !to_process with
                           | [one] -> s "target"
                           | more -> i (List.length more) % s " targets")) in
          [menu_item ~char:'G' ~log `Done]
      in
      go @ [ menu_item ~char:'q' ~log:Log.(s "Cancel") `Cancel ]
      @ (if all_valid_ids = [] then []
         else [ menu_item ~char:'A' ~log:all_log `All; ])
    in
    let sentence =
      if all_valid_ids = [] then Log.(s "Nothing to " % go_verb)
      else Log.(s "Add targets to “" % s list_name % s "”") in
    menu ~sentence ~always_there (target_menu ())
    >>= function
    | `Add id -> to_process := id :: !to_process; loop ()
    | `All -> to_process := all_valid_ids @ !to_process; loop ()
    | `Cancel -> return `Cancel
    | `Done -> return (`Go !to_process)
  in
  loop ()

let make_target_menu ~targets ?(filter_target=fun _ -> true) () =
  List.filter targets ~f:filter_target
  |> sort_target_list
  |> List.map ~f:(fun target ->
      menu_item ~log:Log.(Document.target_for_menu target)
        (`Go (Target.id target)))


let run_with_quit_key action =
  Deferred_list.pick_and_cancel [
    action#start;
    begin
      let rec kbd_loop () =
        Log.(s (quit_key_information ()) @ normal);
        get_key ()
        >>= function
        | 'q' | 'Q' ->
          action#stop;
          return ()
        | _ -> kbd_loop ()
      in
      kbd_loop ()
    end;
  ]
