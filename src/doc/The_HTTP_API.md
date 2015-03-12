When Ketrew Replies on HTTP
===========================

<span style="color: red">Warning: </span>
This is outdated while we work on the next version.

Examples
--------

Let's create a test environment, and source the resulting shell environment:

    ./please.sh test-env
    . _test_env/env.env

Let's start the server:

    kdserver start

You should be able to stop it with

    kdserver stop

Let's add some targets to the database (c.f. `src/test/Workflow_Examples.ml`):

    kdtest website master

You can always browse with the client working like in standalone mode:

    kdclient interact

Let's see the API itself, we use `curl`, here on the `/targets` service:

    curl -k "$ktest_url/targets"

The option `return-error-messages` is set to true, so the server is nice and
says what went wrong:

```badresult
Error: Wrong HTTP Request: Authentication → Insufficient credentials
```

In `_obuild/test-authorized-tokens` there is an “easy token” `"nekot"`:

    curl -k "$ktest_url/targets?token=nekot"

```badresult
Error: Wrong HTTP Request: format-mandatory-parameter → Missing mandatory parameter: "format"
```

Let's try again:

    curl -k "$ktest_url/targets?token=nekot&format=json" | less

Yay we get some Json \o/ i.e. a list of target JSON representations:

```goodresult
[[
  "V0",
  {
    "history": [
      "Successful",
      [
        1410886910.433017,
        [
          "Running",
          [
            {
              "run_history": [
  ...
]
```

We can use `id` parameters to limit the request to one or more
targets:

    one_of_them="ketrew_2014-11-21-19h44m24s850ms-UTC_994326685"
    curl -k "$ktest_url/targets?token=nekot&format=json&id=$one_of_them"

```goodresult
[
   [
      "V0",
      {
         "history" : [
            "Created",
            1408654128.82403
         ],
         "make" : [
            "Direct_command",
            {
               "action" : [
                  "Shell_command",
                  "git add api && git ci -a -m 'update website' "
               ],
               "host" : {
                  "playground" : [
                     "Some",
                     {
                        "kind" : "Directory",
                        "path" : "/tmp"
                     }
                  ],
                  "connection" : "Localhost",
                  "execution_timeout" : "None",
                  "default_shell" : {
                     "command_option" : "-c",
                     "options" : [],
                     "binary" : "None",
                     "command_name" : "sh"
                  },
                  "name" : "file://tmp"
               }
            }
         ],
         "dependencies" : [
            "ketrew_2014-08-21-21h49m48s823ms-UTC_332180439"
         ],
         "persistence" : "Input_data",
         "name" : "Commit",
         "if_fails_activate" : [
            "ketrew_2014-08-21-21h49m48s823ms-UTC_641907500"
         ],
         "id" : "ketrew_2014-08-21-21h49m48s823ms-UTC_781944104",
         "metadata" : "Unit",
         "equivalence" : "Same_active_condition",
         "condition" : "None"
      }
   ]
]
```

By the way, the `/targets` service is
[nullipotent](http://en.wiktionary.org/wiki/nullipotent), and `GET`-only:

    curl -k --data "something to post" "$ktest_url/targets?token=nekot&format=json"

```badresult
Error: Wrong HTTP Request: wrong method → POST
```

What additional queries can we get on the targets?

    curl -k "$ktest_url/target-available-queries?token=nekot&format=json&id=$one_of_them"


we get a list of `("name", "description")` pairs; the queries that are
available for this particular target:

```goodresult
[
  "V0",
  [
    "List_of_query_descriptions",
    [
      [ "stdout", "Stardard output" ],
      [ "stderr", "Stardard error" ],
      [ "log", "Monitored-script `log` file" ],
      [ "script", "Monitored-script used" ],
      [ "check-process", "Check the process-group with `ps`" ]
    ]
  ]
]
```

    curl -k "$ktest_url/target-call-query?token=nekot&format=json&id=$one_of_them&query=log"

```goodresult
[
   "V0",
   [
      "Query_result",
      "start\t2014-11-21 18:44:27\t\nbefore-cmd\t2014-11-21 18:44:27\tCMD0000\tmkdir -p /tmp/deploy_website_ketrew_2014-11-21-19h44m24s848ms-UTC_089809344\nafter-cmd\t2014-11-21 18:44:27\tCMD0000\treturned 0\nbefore-cmd\t2014-11-21 18:44:27\tCMD0001\tcd /tmp/deploy_website_ketrew_2014-11-21-19h44m24s848ms-UTC_089809344\nafter-cmd\t2014-11-21 18:44:27\tCMD0001\treturned 0\nbefore-cmd\t2014-11-21 18:44:27\tCMD0002\tgit clone /Volumes/Encrypted_zzz/dev/ketrew\nafter-cmd\t2014-11-21 18:44:28\tCMD0002\treturned 0\nsuccess\t2014-11-21 18:44:28\t\n"
   ]
]
```

