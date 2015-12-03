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

