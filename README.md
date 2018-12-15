# dwproxy

This is a client-agnostic proxy for the Discworld MUD, which adds new
commands:

- `speedwalk [from <location>] to <location>`: find routes, walk if
  there's only one
- `route <n>`: select a speedwalk route option
- `shop <item>`: search shops by items they sell

It uses the database from [Quow's Cow Bar and
Minimap](http://quow.co.uk/minimap.php), which should be available,
and the path to which should be provided as the first argument on
invocation, e.g.: `dwproxy ~/discworld/quow_cowbar/`.

The default port is 2000, `telnet localhost 2000` to connect.
