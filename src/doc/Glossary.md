Glossary
========

Here are some common terms used in this documentation.

EDSL Concepts
-------------

A **node** (function `Ketrew.EDSL.workflow_node`) is the basic building block of
a workflow (it is often also called a “target”). A workflow-node has many
components:

- A **build-process** describes how a program should be run, usually on a given
  host; for example the function `Ketrew.EDSL.pbs` returns a datastructure
  representing the action of running a program on a host with the PBS batch
  scheduler. It is the parameter `?make` of `Ketrew.EDSL.workflow_node`.
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
- A **condition** (`Ketrew.EDSL.Condition.t`) defines how to tell if a node
  should be run or not (it is set by the “product” of the node or by the
  argument `?done_when` of `Ketrew.EDSL.workflow_node`).
- Links to other nodes, i.e., **edges**:
    - `Ketrew.EDSL.depends_on` are nodes that need to be satisfied or run before a
      node can start running,
    - `Ketrew.EDSL.on_failure_activate` are nodes that will be activated if the
      node fails (being killed is considered failing here), and
    - `Ketrew.EDSL.on_success_activate` are nodes that will be activated only
      *after* a node succeeds.
- **Metadata** and **tags** are user-provided information; the `?metadata` is,
  for now, just a raw string; the `?tags` are a list of strings which are
  “searchable” (see “filters” in the UI).
- An **Equivalence** specification tells the engine when to consider a node
  redundant, the default value is the most common and obvious,
  `` `Same_active_condition``:<br/>
    - when adding a node to the engine, if an active or activable node already
      has the same *condition* then the new one is considered redundant and will
      be replaced with a pointer to the older one;
    - the behavior above can be cancelled by providing `` `None``
      (cf. `Ketrew_pure.Target.Equivalence.t`).

In the EDSL, a workflow-node has a **product** which defines, via its `#is_done`
method, how the *condition* of the node is tested.  The product of a node can be
accessed through the `#product` method.


Ketrew's Engine
---------------

Ketrew's **engine** is the process in charge of orchestrating and monitoring
the run of the workflows.
The engine is run by the server; the client applications “submit” workflows.

The engine runs long-running processes through **plugins**
(see [documentation](src/doc/Long-Running_Plugins.md)).

