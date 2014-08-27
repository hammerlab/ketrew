
let name = "dummy"

let create ~host program =
  let `Long_running  (_, serialized) =
    Ketrew_daemonize.create ~using:`Python_daemon ~host program in
  `Long_running (name, serialized)

module Another_long_running : Ketrew_long_running.LONG_RUNNING = struct
  include Ketrew_daemonize
  let name = name
end

let () =
  Ketrew_state.register_long_running_plugin ~name (module Another_long_running)


