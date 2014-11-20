Glossary
========

Here are some common terms used in this documentation.

EDSL Concepts
-------------

A **target** (`Ketrew_edsl.target`) is the basic building block of a
workflow, a target has many components:

- A **build-process** wraps a way for a target of running a program, usually on
a given host; for example the function `Ketrew_edsl.pbs` returns a
datastructure representing the action of running a program on a host with the
PBS batch scheduler.
    - A **host** (`Ketrew_edsl.Host.t`) is a place where one can run things.
    Usually a host is an SSH host with a “playground”, and a few other options.
    - A **playground** is a directory on a host where the Ketrew engine and its
    plugins can (and will) write log files, temporary scripts, etc.
    - A **program** (`Ketrew_edsl.Program.t`) is a datastructure representing
    shell scripts on steroids, Ketrew provides high-level combinators to build
    programs.
- A **condition** (`Ketrew_edsl.Condition.t`) defines how to tell if a target
should be run or not (the argument `?done_when` of `Ketrew_edsl.target`).
- Links to other targets:
    - **dependencies** are targets that need to be ensured or
    run before a target can start,
    - **fallbacks** targets that will be activated if the target fails, and
    - **success-triggers** targets that will be activated only *after* a target
    succeeds.


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

