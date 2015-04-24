When Ketrew Replies on HTTP
===========================

Examples
--------

Let's create a test environment, and source the resulting shell environment:

    ./please.sh test-env
    . _test_env/env.env

Let's start the server:

    kdserver start

You should be able to stop it with

    kdserver stop

Let's add some targets to the database (cf. the
[workflow examples](src/test/Workflow_Examples.ml)):

    kdtest two-py /tmp "du -sh $HOME" "du -sh $PWD"

You can always browse with the client working like in standalone mode:

    kdclient interact

Let's see the API itself, we use `curl`, and we `POST` messages to the `/api`
service:

    json='["V0", [ "Get_targets", [] ] ]'
    curl -d "$json" -k "$ktest_url/api?token=nekot"

```goodresult
[ "V0",
  [
    "List_of_targets",
    [
      {
        "tags": [ "website", "end-of-workflow" ],
        "log": [],
        "history": [
          "Finished",
...
```

where:

- The variable `$ktest_url` is provided in `_test_env/env.env`.
- The parameter `token` is an authentication token (the server reads the file
  `_test_env/test-authorized-tokens`, where there is an “easy token” `"nekot"`).
- The `POST` data (`-d $json`) is the serialized up-message.  The available
  messages are defined in `src/pure/ketrew_protocol.mli` and tagged with a
  version.

To find out how to give the right JSON to to the API, you may just check from
the top-level:

```ocaml
utop[0]> #require "ketrew";;
utop[1]> module K = Ketrew_protocol.Up_message;;
module K = Ketrew_protocol.Up_message
utop[2]> K.serialize;;
- : K.t -> bytes = <fun>
utop[3]> K.serialize (`Get_targets []);;
- : bytes = "[ \"V0\", [ \"Get_targets\", [] ] ]"
```

