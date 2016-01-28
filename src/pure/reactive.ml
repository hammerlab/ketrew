
(* Trying to wrap React and ReactiveData stuff *)

type 'a signal = 'a React.S.t
type 'a signal_list_wrap = 'a ReactiveData.RList.t

module Signal = struct

  type 'a t = 'a React.S.t

  let map s ~f = React.S.map f s
  let value s = React.S.value s

  let bind s ~f = React.S.bind s f
  let constant s = React.S.const s

  let singleton t =
    let open ReactiveData.RList in
    from_event
      [React.S.value t]
      (React.E.map (fun e -> Set [e]) (React.S.changes t))

  let list t =
    let open ReactiveData.RList in
    from_event
      (React.S.value t)
      (React.E.map (fun e -> Set e) (React.S.changes t))

  let tuple_2 a b =
    React.S.l2 (fun a b -> (a, b)) a b

  let tuple_3 a b c =
    React.S.l3 (fun a b c -> (a, b, c)) a b c
end


module Source = struct
  type 'a t = {
    signal: 'a React.S.t;
    set: 'a -> unit;
  }

  let create ?eq v =
    let signal, set = React.S.create ?eq v in
    {signal; set}

  let set t v = t.set v

  let signal t = t.signal

  let value t = t.signal |> React.S.value

  let modify_opt t ~f =
    match f (value t) with
    | Some s -> t.set s
    | None -> ()

  let modify t ~f = modify_opt t ~f:(fun v -> Some (f v))

  let map_signal t ~f = Signal.map t.signal ~f


end

