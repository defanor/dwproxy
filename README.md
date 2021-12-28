# dwproxy

This is a client-agnostic proxy for the Discworld MUD, which adds new
commands:

- `speedwalk [from <location>] to <location>` or `sw <destination>`:
  find routes, walk if there's only one
- `<number>`: select a speedwalk route option
- `shop <item>`: search shops by items they sell

It uses the database from [Quow's Cow Bar and
Minimap](http://quow.co.uk/minimap.php), which should be available,
and the path to which should be provided as the first argument on
invocation, e.g.: `dwproxy _quowmap_database.db`.

The default port is 2000, `telnet localhost 2000` to connect.

Can be built with either cabal (`cabal install`) or plain GHC on
Debian 11, after installing the compiler and dependencies from system
repositories (`ghc libghc-fgl-dev libghc-unordered-containers-dev
libghc-network-dev libghc-async-dev libghc-attoparsec-dev
libghc-aeson-dev libghc-hdbc-sqlite3-dev`).


## Python version

There's a Python version, dwproxy.py. It runs on Python 3, and doesn't
have dependencies outside of the Python standard library.
