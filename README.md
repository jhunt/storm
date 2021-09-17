STORM
=====

This is a small Twitter application, written in Common Lisp, that
takes longform text (with optional media references) and slices it
up into numbered tweets, threads them, and spaces them out to
emulate typists of varying speeds.

For this to work, you need to get [chirp][chirp] spun up with a
custom application, and authenticated to the Twitter account you
want to send tweets as.

[chirp]: https://github.com/Shinmera/chirp


Running Storm
-------------

_There's a tweetstorm a-brewin'..._

Storm is fully dockerized.  Out-of-the-box, the
[iamjameshunt/storm][img] does the following:

  1. Read and write configuration from `/etc/storm/settings`,
  2. Watch `/tweets` for files named `*.tweet`, and
  3. Tweet those files at 10 words per minute

This means you usually want to mount in volumes for those, either
named Docker volumes or (more preferred) bind-mount directories,
à la:

```sh
docker run -v /srv/storm/etc:/etc/storm \
           -v /srv/storm/tweets:/tweets \
           iamjameshunt/storm
```

While configuration is _stored_ in /etc/storm, out of the box,
there won't be any configuration to read.  In this mode, Storm is
operating **unauthenticated**.

You can fix this by either creating a settings file manually, like
this:

```sh
cat > /srv/storm/etc/settings <<EOF
((api-key       . "YOUR-API-KEY")
 (api-secret    . "YOUR-API-SECRET")
 (access-token  . "OAUTH-ACCESS-TOKEN")
 (access-secret . "OAUTH-ACCESS-SECRET"))
EOF
```

or, by Swank (via Emacs SLIME, or some other route) to connect to
the live REPL (read-eval-print loop) running inside of the
container.

(emacs gif)

For this to work, you need to be forwarding the container's
TCP/4005 port to something reachable.  For local dev, this works:

```sh
docker run -p 40050:4005 iamjameshunt/storm
```

For remote work, I normally recommend forwarding _only_ on
localhost / loopback, and setting up an SSH tunnel to protect the
Swank connection:

```sh
james@prod # docker run -d -p 127.0.0.1:40050:4005 \
                        iamjameshunt/storm

james@laptop $ ssh -N james@prod -L 40050:127.0.0.1:40050

james@laptop $ emacs   # M-x slime-connect etc...
```

Once you are connected to the container's REPL, you can issue the
following Common Lisp calls to kick off authentication,
interactively:

```lisp
CL-USER> (chirp:initiate-authentication
            :api-key    "YOUR-API-KEY"
            :api-secret "YOUR-API-SECRET")
"https://twitter-auth-url-or-something/?AAAAzg12h3gjhasd"

CL-USER> (chirp:complete-authentication "PIN-FROM-BROWSER")

CL-USER> (save-chirp-auth)
```

From then on (assuming the `/etc/storm` directory persists across
container restarts) everything should **Just Work**(TM).

[img]: https://hub.docker.com/r/iamjameshunt/storm
