
open Ketrew_pure
open Internal_pervasives

module Target_cache  = struct
  type target_knowledge = [
    | `None
    | `Summary of Target.Summary.t
    | `Pointer of Target.id * Target.Summary.t
  ]
  type flat_state = {
    retrieved: Time.t option;
    value: Target.State.Flat.t;
  }
  type query_description = [
    | `None
    | `Descriptions of (string * string) list
  ]
  type query_result = [
    | `None
    | `String of (Time.t * string)
    | `Error of string
  ]
  type t = {
    targets: (Target.id, target_knowledge Reactive.Source.t) Hashtbl.t;
    flat_statuses: (Target.id, flat_state Reactive.Source.t) Hashtbl.t; 
    backend_query_descriptions: (Target.id, query_description Reactive.Source.t) Hashtbl.t;
    backend_query_results: (Target.id * string, query_result Reactive.Source.t) Hashtbl.t;
  }

  let create () = {
    targets = Hashtbl.create 42;
    flat_statuses = Hashtbl.create 42;
    backend_query_descriptions = Hashtbl.create 42;
    backend_query_results = Hashtbl.create 42;
  }

  let markup_counts t =
    let open Display_markup in
    let count n tb = n, textf "%d" (Hashtbl.length tb) in
    description_list [
      count "Target-summaries" t.targets; 
      count "Flat-statuses" t.flat_statuses; 
      count "Query-descriptions" t.backend_query_descriptions; 
      count "Query-results" t.backend_query_results; 
    ]

  let _get_target_knowledge {targets; _} ~id =
    try (Hashtbl.find targets id)
    with _ ->
      (* Log.(s "Target-cache: miss on " % s id @ verbose); *)
      let signal = Reactive.Source.create `None in
      (* Log.(s "Created `None` target signal for " % s id @ verbose); *)
      Hashtbl.replace targets id signal;
      signal

  let _get_target_flat_status {flat_statuses; _} ~id =
    try (Hashtbl.find flat_statuses id)
    with _ ->
      let source =
        Reactive.Source.create
          {retrieved = None; value = Target.State.Flat.empty ()} in
      Hashtbl.replace flat_statuses id source;
      source

  let _get_target_query_descriptions {backend_query_descriptions; _} ~id =
    try (Hashtbl.find backend_query_descriptions id)
    with _ ->
      let source = Reactive.Source.create `None in
      Hashtbl.replace backend_query_descriptions id source;
      source

  let _get_target_query_result {backend_query_results; _} ~id ~query =
    try (Hashtbl.find backend_query_results (id, query))
    with _ ->
      let source = Reactive.Source.create `None in
      Hashtbl.replace backend_query_results (id, query) source;
      source

  let rec get_target_summary_signal t ~id =
    let open Reactive in
    _get_target_knowledge t ~id
    |> Source.signal

  let get_target_flat_status_signal t ~id =
    _get_target_flat_status t ~id
    |> Reactive.Source.signal
    |> Reactive.Signal.map ~f:(fun x -> x.value)

  let get_target_query_descriptions t ~id =
    _get_target_query_descriptions t ~id
    |> Reactive.Source.signal

  let get_target_query_result t ~id ~query =
    _get_target_query_result t ~id ~query
    |> Reactive.Source.signal

  let get_target_flat_status_last_retrieved_time t ~id =
    _get_target_flat_status t ~id
    |> Reactive.Source.signal
    |> Reactive.Signal.value
    |> (fun x -> x.retrieved)

  let update_target t ~id new_value =
    let source = _get_target_knowledge t id in
    Reactive.Source.set source new_value;
    (* Hashtbl.replace targets id signal; *)
    ()

  let update_flat_state t ~server_time ~id more_state =
    let source = _get_target_flat_status t ~id in
    let current = Reactive.(Source.signal source |> Signal.value) in
    let value = Target.State.Flat.merge current.value more_state in
    let retrieved = Some server_time in
    Reactive.Source.set source {retrieved; value};
    ()

  let update_target_query_descriptions t ~id v =
    let source = _get_target_query_descriptions t ~id in
    Reactive.Source.set source v;
    ()

  let update_target_query_result t ~id ~query result =
    let source = _get_target_query_result t ~id ~query in
    Reactive.Source.set source  result;
    ()


  let clear {targets; flat_statuses;
             backend_query_results; backend_query_descriptions} =
    Hashtbl.clear targets;
    Hashtbl.clear flat_statuses;
    Hashtbl.clear backend_query_results;
    Hashtbl.clear backend_query_descriptions;
    ()


end
