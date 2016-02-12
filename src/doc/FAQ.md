Frequently Asked Questions
==========================

These are some more specific explanations that can be useful to read. If you
have more questions submit a Github
[issue](https://github.com/hammerlab/ketrew/issues) (with the
[“questions”](https://github.com/hammerlab/ketrew/issues?utf8=%E2%9C%93&q=+is%3Aissue+label%3Aquestion+)
label).

Optional `?make` Argument
-------------------------

> Why is `?make`, the `Build_process.t` argument, optional in
> `EDSL.workflow_node`?<br/>
> What's the use case for having "no-ops" as default targets?

There are many use cases, 

- one may want to make sure a condition is there and fail early to tell the user
  (like some software configuration that requires human intervention, e.g. using
  [`google/skicka`](https://github.com/google/skicka) for backups requires
  setting up authentication manually),
- a target can be used only to put dependencies together (that's often the case
  to submit a common ancestor for a bunch of stuff to do, for instance, see
  [Biokepi's demo](https://github.com/hammerlab/biokepi/blob/0739e43d31ea62167716f96ef5ffa4a6891f5669/src/app/main.ml#L179)),
  or
- one may want to treat input data in a uniform way (“everything is a workflow
  node”
  [there](https://github.com/hammerlab/biokepi/blob/master/src/lib/pipeline.ml#L454)).

Trakeva Missing Backends
------------------------

While configuring Ketrew and trying to start a server it may fail with:

```
[ketrew: ERROR]
    The "sqlite" backend was not available at Trakeva's compile time (available
    backends: [ ...  ])
```

It means that Trakeva was not compiled with support for the database backend
you are requesting via the [config-file](src/doc/The_Configuration_File.md).

Solutions:

- Read the documentation about [DB Backends](src/doc/Database_Backends.md).
- Use another backend (the error message tells which ones are available,
  between brackets, maybe empty).
- Install the backend you want:
    - `opam install postgresql` or `opam install sqlite3` (`opam` will then
      catch the change and recompile `trakeva` and `ketrew`).
    - If you are installing `sqlite3` on MacOSX you should read
      [this](https://github.com/smondet/trakeva#sqlite3-on-macosx).

Which Dependency Failed?
------------------------

Sometimes you submit a 2000-nodes pipeline, and when it fails you want to see
the node that “really” failed; i.e. that didn't fail because one of its
dependencies failed.

Solution: use `(and (is-failed) (not (is-dependency-dead)))` as part of the
filter in the WebUI.

There is an example ready to try in the Help section:

![Screen Shot Of The Filter's Help Section](https://cloud.githubusercontent.com/assets/617111/12961777/261356d0-d010-11e5-9619-9e02a7efe08b.png)

See also more discussion at issue
[`#345`](https://github.com/hammerlab/ketrew/issues/345).

