Ketrew: Keep Track of Experimental Workflows
============================================

This is **Work in Progress**, not ready for use.

This documentation is available at <http://seb.mondet.org/ketrew/>,
the source is at <https://github.com/smondet/ketrew/>.

Build & Install
---------------

### From Opam

If you have `opam` up and running:

    opam remote add smondet git@github.com:smondet/dev-opam-repo
    opam install ketrew



Usage
-----

### Initialization

To create a configuration file, run:

    ketrew-client init

This creates `$HOME/.ketrew/client.toml` (see `ketrew-client init --help` to
choose another path).

### Workflow Scripts

A workflow script is an OCaml file (script, compiled, or even inside the
toplevel).

One can add functions that create and `run` workflows:

```ocaml
let run_command_with_lsf cmd =
  let open Ketrew.EDSL in
  let host = 
    parse_host "ssh://user42@MyLSFCluster/home/user42/ketrew-playground/?shell=bash" in
  let queue = "normalpeople" in
  let program = Program.sh cmd in
  run (
    target "run_command_with_lsf"
      ~make:(lsf ~queue ~wall_limit:"1:30" ~processors:(`Min_max (1,1)) ~host program)
  )

let () = (* Extremely basic main of the workflow script *)
  run_command_with_lsf Sys.argv.(1)
```

then one should be able to add and activate mini-workflow with:

    <script.ml> "<some shell command to run on the cluster>"


See the file `src/test/cli.ml` for more examples (*work-in-progress*),
and the [documentation of the EDSL API](src/lib/ketrew_edsl.mli).


### Ketrew-client

Ketrew is driven from the command-line client.  See:

    ketrew-client --help

To display the current status:

    ketrew-client status

To run as many steps as possible until a “fix-point” is reached:

    ketrew-client run fix

To kill running jobs use

    ketrew-client kill <target-ID>

or do an interactive murder:

    ketrew-client kill --interactive

Once targets are dead, one can “archive” them, i.e. put them in a less-visible
list.

    ketrew-client archive [--interactive] [<target-ID>]*

See also `ketrew-client interact` or `ketrew-client explore` for fun
*one-key-based* navigation.


