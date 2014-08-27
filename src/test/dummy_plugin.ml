
open Ketrew_pervasives

let name = "dummy"

let create ~host program =
  let `Long_running  (_, serialized) =
    Ketrew_daemonize.create ~using:`Python_daemon ~host program in
  `Long_running (name, serialized)

module Another_long_running : Ketrew_long_running.LONG_RUNNING = struct
  include Ketrew_daemonize
  let name = name

  let additional_queries run_param =
    ("date", Log.(s "Display the date, not even on the right host"))
    :: additional_queries run_param

  let query run_param item =
    if item = "date" then
      begin Ketrew_host.(get_shell_command_output (of_string "/tmp")
                           (fmt "date"))
        >>< function
        | `Ok (o, _) -> return o
        | `Error e ->
          fail Log.(s "Command `date` failed: " % s (Ketrew_error.to_string e))
      end
    else (* call Ketrew_daemonize's function: *)
      query run_param item
end

let () =
  Ketrew_state.register_long_running_plugin ~name (module Another_long_running)


