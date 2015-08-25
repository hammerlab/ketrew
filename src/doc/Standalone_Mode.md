The Standalone Mode
===================

If you configured Ketrew with `ketrew init`, a standalone profile is already
configured with the same database as the client/server version (both cannot be
used at the same time).

    ketrew stop -P daemon # stopping the daemon if it running
    ketrew status  -P standalone 

Let's submit again a simple command:

    ketrew submit  -P standalone  --daemonize "/tmp/KT","du -sh $PWD" -w
    
Run one “fix-point” and then check the status:

    ketrew run fix -P standalone
    ketrew status  -P standalone 

The job has started in the background now:
    
```goodresult
[ketrew] Current targets “in-progress”: 1
```

but Ketrew itself cannot check on it, you can keep running it manually, for
example until there is nothing left to do:

    ketrew run loop -P standalone

```goodresult
[ketrew] Press the 'q' key to stop.
```

There is no “Web UI” for the standalone mode, but you can still inspect the
state with the textual interface:

    ketrew interact -P standalone

