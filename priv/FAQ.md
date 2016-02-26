FAQ
===

Why?
----

Why not.

What the difference from ...?
-----------------------------

1. Concurrency and scalability as properties of Erlang platform.
2. Simplicity. There is no user registration nor authorization, and there are only
few core operations and it is going to stay this way.
3. Ease of deployment (25mb docker image, minimum dependencies).
4. Built-in replication and https support.

What is it for (possible use cases)?
====================================

Data storage
------------

Data could be stored from injected into browser javascript application (instead of localStorage),
and later used from other places.

Service discovery
-----------------

Keys with TTL allows to organize simple service discovery: services register itself at startup
and update their registration regularly while service rounter uses this registry to route requests.
Example is a number of REST microservices behind a common entry point.

Event source
------------

Javascript application could subscribe to key update events, and do something when event happens.
Also applicable to server side actions, e.g. a server doing something when client updates the key,
like putting data to some TSDB (time-series database) to have nice graphs.

Possibly other
--------------

Impossible is nothing, so to say. Suggestions are welcome.