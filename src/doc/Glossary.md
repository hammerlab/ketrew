Glossary
========

Here are some common terms used in this documentation.

EDSL Concepts
-------------

A **target** (function `Ketrew.EDSL.target`) is the basic building block of a
workflow, a target has many components:

- A **build-process** wraps a way for a target of running a program, usually on
  a given host; for example the function `Ketrew.EDSL.pbs` returns a
  datastructure representing the action of running a program on a host with the
  PBS batch scheduler. It is the parameter `?make` of `Ketrew.EDSL.target`.
    - A **host** (`Ketrew.EDSL.Host.t`) is a place where one can run things.
      Usually a host is an SSH host with a “playground”, and a few other
      options.
    - A **playground** is a directory on a host where the Ketrew engine and its
      plugins can (and will) write log files, temporary scripts, etc.
      Most features which use hosts require playgrounds and will fail if not
      provided.
    - A **program** (`Ketrew.EDSL.Program.t`) is a datastructure representing
      shell scripts on steroids, Ketrew provides high-level combinators to build
      programs.
- A **condition** (`Ketrew.EDSL.Condition.t`) defines how to tell if a target
  should be run or not (the argument `?done_when` of `Ketrew.EDSL.target`).
- Links to other targets:
    - **dependencies** are targets that need to be satisfied or run before a
      target can start,
    - **on-failure-activate** targets that will be activated if the target fails, and
    - **on-success-activate** targets that will be activated only *after* a target
    succeeds.
- **Metadata** and **tags** are user-provided information; the `?metadata` is,
  for now, just a raw string; the `?tags` are a list of strings which are
  “searchable” (see “filters” sub-menu in the test-user-interface).
- An **Equivalence** specification tells the engine when to consider a target
  redundant, the default value is the most common:<br/>
    - when adding a target, if an active or activable target already has the
      same *condition* then the new one is considered redundant and will be
      replaced with a pointer to the older one;
    - the behavior above can be cancelled by providing `` `None``
      (cf. `Ketrew_pure.Target.Equivalence.t`).

In the EDSL, a target may have a **product**, which is, for now, just a facility
for the EDSL itself (the engine does not keep track of products).
The `?product` argument can be used to track file-paths within your EDSL
program (see for example the function `Ketrew.EDSL.file_target` which
just sets the product and the condition of a target for a given file-path; the
path can be retrieved with `my_target#product#path`).


Ketrew's Engine
---------------

Ketrew's **engine** is the process in charge of orchestrating and monitoring
the run of the workflows.

- In **standalone** mode, the command line application has to run the
engine manually.
- In **client-server** mode, the engine is run by the server; the client
applications just “submit” workflows.

The engine runs long-running processes through **plugins**
(see [documentation](src/doc/Long-Running_Plugins.md)).

