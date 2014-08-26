When Ketrew Replies on HTTP
===========================


Examples
--------

Let's create a test environment, and source the resulting shell environment:

    ./please.sh test-env
    . _obuild/test.env

Let's add some targets to the database (c.f. `src/test/cli.ml`):

    kttest website

Now we can start the server:

    ktapp start-server

The sever can be killed anytime with:

    ktkillserver

We use `curl`, here on the `/targets` service:

    curl -k "$ktest_url/targets"

The option `return-error-messages` is set to true, so the server is nice and
says what went wrong:

```badresult
Error: Wrong HTTP Request: Authentication → Insufficient credentials
```

In `_obuild/test-authorized-tokens` there is an “easy token” `"nekot"`:

    curl -k "$ktest_url/targets?token=nekot"

```badresult
Error: Wrong HTTP Request: format-parameter → Missing parameter
```

Let's try again:

    curl -k "$ktest_url/targets?token=nekot&format=json"

Yay we get some Json \o/ i.e. a list of target-identifiers.

```goodresult
[
  "ketrew_2014-08-21-21h49m48s823ms-UTC_994326685",
  "ketrew_2014-08-21-21h49m48s823ms-UTC_930807020",
  "ketrew_2014-08-21-21h49m48s823ms-UTC_781944104",
  "ketrew_2014-08-21-21h49m48s823ms-UTC_641907500",
  "ketrew_2014-08-21-21h49m48s823ms-UTC_366831641",
  "ketrew_2014-08-21-21h49m48s823ms-UTC_332180439",
  "ketrew_2014-08-21-21h49m48s823ms-UTC_290180182"
]
```

We can use `id` parameters to expand the Json serialization of one or more
targets:

    one_of_them="ketrew_2014-08-21-21h49m48s823ms-UTC_781944104"
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



<!--
" Vim stuff:

let $ktest_url="https://localhost:8443"
nmap <leader>I 0mki:read !<esc><leader>x`k07x
nmap <leader>E Ilet $<esc>V<leader>xu

-->
