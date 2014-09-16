When Ketrew Replies on HTTP
===========================


Examples
--------

Let's create a test environment, and source the resulting shell environment:

    ./please.sh test-env
    . _obuild/test.env

Let's start the server:

    kdserver start

You should be able to stop it with

    kdserver stop

Let's add some targets to the database (c.f. `src/test/cli.ml`):

    kdtest website some_branch

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

    curl -k "$ktest_url/targets?token=nekot&format=json"

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

    one_of_them="ketrew_2014-09-16-18h02m29s828ms-UTC_290180182"
    curl -k "$ktest_url/targets?token=nekot&format=json&id=$one_of_them"

or with a pretty-printer:

    curl -k "$ktest_url/targets?token=nekot&format=json&id=$one_of_them" | json_pp

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
         "persistance" : "Input_data",
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
  [ "stdout", "Stardard output" ],
  [ "stderr", "Stardard error" ],
  [ "log", "Monitored-script `log` file" ],
  [ "script", "Monitored-script used" ]
]
```

    curl -k "$ktest_url/target-call-query?token=nekot&format=json&id=$one_of_them&query=log" | json_pp

```goodresult
[
  "start\t2014-09-05 19:09:01\t\nbefore-cmd\t2014-09-05 19:09:01\tCMD0000\tcd /tmp/deploy_website_ketrew_2014-09-05-20h06m11s022ms-UTC_089809344/ketrew\nafter-cmd\t2014-09-05 19:09:01\tCMD0000\treturned 0\nbefore-cmd\t2014-09-05 19:09:01\tCMD0001\tbash _/please_sh clean build doc\nafter-cmd\t2014-09-05 19:09:14\tCMD0001\treturned 0\nsuccess\t2014-09-05 19:09:14\t\n"
]
```

