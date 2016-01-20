open Ketrew_pure.Internal_pervasives
open Unix_io

(* A parametric circular buffer. *)
module Ring = struct
  type 'a t = {
    recent: 'a option array;
    mutable index: int;
  }

  let create ?(size = 4200) () =
    {recent = Array.make size None; index = 0}

  let add t v =
    t.recent.(t.index) <- Some v;
    t.index <- (t.index + 1) mod (Array.length t.recent)

  let fold_right t ~init ~f =
    let l = Array.length t.recent in
    let p i = (i - 1 + l) mod l in
    let rec go count prev index =
      let next item = f prev item in
      match index, t.recent.(index) with
      | _, None -> prev
      | _, _ when count = l -> prev
      | n, Some s ->
        go (count + 1) (next s) (p n)
    in
    go 0 init (p t.index)

  let take t max =
    fold_right ~init:(0, []) t ~f:(fun (count, l) item ->
        if count >= max
        then (count, l)
        else (count + 1, item :: l))
    |> snd

  
  let clear t =
    t.index <- 0;
    Array.iteri t.recent  ~f:(fun i _ -> t.recent.(i) <- None)

end (* Ring *)

module Log_store = struct
  type t = Typed_log.Item.t Ring.t

  let create ?size () : t =
    Ring.create ?size ()

  let add t l = Ring.add t l

  let append_to_file ~path ~format t =
    let as_list = Ring.fold_right ~init:[] t ~f:(fun prev x -> x :: prev) in
    IO.with_out_channel (`Append_to_file path) ~f:(fun out ->
        IO.write out (if format = `Json then "[\n" else "")
        >>= fun () ->
        List.fold ~init:(return true) as_list
          ~f:(fun prev_m item ->
              prev_m >>= fun prev ->
              IO.write out
                (if format = `Json then
                   (if prev then "\n" else ",\n")
                 else "\n" ^ String.make 80 '-' ^ "\n")
              >>= fun () ->
              IO.write out
                (match format with
                | `Json ->
                  Yojson.Safe.pretty_to_string (Typed_log.Item.to_yojson item)
                | `Txt ->
                  Typed_log.Item.show item)
              >>= fun () ->
              return false
            )
        >>= fun (_ : bool) ->
        IO.write out (if format = `Json then "\n]\n" else "\n")
      )

  let clear t =
    Ring.clear t;
    return ()

end (* Log_store *)

module Global = struct

  let _log = Log_store.create ()

  module Logger = struct
    include Typed_log.Item.Construct
    include Display_markup
    let add l =
      Log_store.add _log l
    let log l = add (now l)

  end
  module Make_module_error_and_info(M: sig val module_name : string end) = struct
    let log_error e lo =
      let open Logger in
      let open Typed_log.Item in
      description_list [
        Constants.word_module, text M.module_name;
        Constants.word_error, text (Error.to_string e);
        Constants.word_info, text (Log.to_long_string lo);
      ] |> log
    let log_info lo =
      let open Logger in
      let open Typed_log.Item in
      description_list [
        Constants.word_module, text M.module_name;
        Constants.word_info, text (Log.to_long_string lo);
      ] |> log
    let log_markup mu =
      let open Logger in
      let open Typed_log.Item in
      description_list (
        (Constants.word_module, text M.module_name) :: mu
      ) |> log

  end

  let append_to_file ~path ~format = Log_store.append_to_file ~path ~format _log
  let clear () = Log_store.clear _log

end (* Global *)

module User_level_events = struct
  type item = {
    date: Time.t;
    event: [
      | `Workflow_received of string list * int
    ]
    } [@@deriving yojson]

  let event_to_string =
    function
    | `Workflow_received ([one], count) ->
      fmt "Workflow received: %s (%d node%s)"
        one count
        (match count with 1 -> "" | _ -> "s")
    | `Workflow_received (more, count) ->
      fmt "Multi-Workflow received: %s (%d node%s)"
        (String.concat ~sep:", " more) count
        (match count with 1 -> "" | _ -> "s")

  type t = {
    ring: item Ring.t;
    signal_changes: unit -> unit;
    changes: unit React.E.t;
  }
  let _global : t =
    let changes, signal_changes = React.E.create () in
    {
      changes; signal_changes; 
      ring = Ring.create ~size:1000 ();
    }

  let item ?date event =
    let date =
      match date with
      | None -> Time.now ()
      | Some d -> d in
    {date; event}
    
  let workflow_received ~names ~count =
    Ring.add _global.ring (item (`Workflow_received (names, count)));
    _global.signal_changes ()

  let get_notifications_or_block ~query =
    begin match query with
    | None ->
      return (Ring.take _global.ring 10)
    | Some since ->
      wrap_deferred
        ~on_exn:(fun e -> `Failure ("User_level_events" ^ Printexc.to_string e))
        Lwt.(fun () ->
            let rec try_or_wait () =
              let result =
                Ring.fold_right _global.ring ~init:[] ~f:(fun l item ->
                    if item.date > since
                    then item :: l
                    else l)
                |> List.rev in
              match result with
              | [] ->
                Lwt.pick [
                  (Lwt_react.E.next _global.changes >>= fun () -> return `Event);
                  (Lwt_unix.sleep 30. >>= fun () -> return `Timeout);
                ]
                >>= begin function
                | `Event -> try_or_wait ()
                | `Timeout -> return []
                end
              | _ :: _  ->
                return result
            in
            try_or_wait ())
    end
    >>= fun result ->
    return (List.map result ~f:(fun {date; event} ->
        date, event_to_string event))


  
end
