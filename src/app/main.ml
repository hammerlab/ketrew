
let `Never_returns =
  let db_file = "/path/to/ketrew_database" in
  let configuration = Ketrew_state.Configuration.create db_file () in
  Ketrew.Command_line.run_main ~configuration ()
