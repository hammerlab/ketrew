open Ketrew_pervasives

type t = [
  | `Fail of Log.t
  | `Make of Ketrew_target.t * Ketrew_target.t list
]

let log = function
| `Fail l -> Log.(s "Fail with: " % brakets l)
| `Make (act, deps) -> 
  Log.(s "Make target :" % Ketrew_target.log act
       % (match deps with
         | [] -> empty
         | more ->
           parens (s "with " % separate (s ", ") 
                     (List.map ~f:Ketrew_target.log more))))

let run_list ~state todo_list =
  Deferred_list.while_sequential todo_list (function
    | `Make (active, dependencies) ->
      begin 
        Deferred_list.while_sequential (active :: dependencies) (fun t ->
            Ketrew_state.add_target state t)
        >>= fun (_ : unit list) ->
        return ()
      end
    | `Fail l ->
      Log.(s "Fail: " % l @ error);
      fail (`Failure (Log.to_long_string l)))
  >>= fun (_ : unit list) ->
  return ()
