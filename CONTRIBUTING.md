Contributing to Ketrew
======================

Issues/Questions
----------------

The best place to discuss Ketrew is within Github's
[issues](https://github.com/hammerlab/ketrew/issues) (used even for questions).

Contributions
-------------

We follow the usual pull-requests mechanism, trying to have meaningful and
readable commits (which all at least compile).

*Before* working on something, submit an issue if there is not already one; and
comment on the issue your intentions.
Then mention the issue in your commit messages.

Before submitting a PR, please check that the tests still run, and the
documentation builds.

We don't `git push -f`, or use `rebase` once a PR is started, please address
peer-review comments by adding commits to your PR (pushing to same branch on
your repository).


OCaml Guidelines
----------------

Please refer to our
[OCaml Guidelines](https://github.com/hammerlab/style-guides/blob/master/ocaml.md).

On top of that for Ketrew:

- We uses a pervasive Error monad embedded in `Lwt`
(see [`pvem_lwt_unix`](http://seb.mondet.org/software/pvem_lwt_unix/index.html))
for all the *library* (i.e. internal) code. This makes type signatures scary as
they ought to be, and forces self-documenting code discipline.
- But *in contrast*, the EDSL should be much simpler, in order to be picked up
  by OCaml-beginners.

More information for developers is available in the
[documentation](src/doc/Developer_Documentation.md).
