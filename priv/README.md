Public KV
=========

FAQ
---

Please see [FAQ](priv/FAQ.md)

Command line API
----------------

```sh
KV=https://kv.jamhed.tk
UUID=`curl -s $KV/uuid`
curl -XPUT -d some-data $KV/key/$UUID/some-key
# returns ok
curl $KV/key/$UUID/some-key
# returns some-data
```

In browser API
--------------

Suppose you want to store something from script running in browser, and then retrieve
the value from somewhere else. This is a primitive key-value store exactly for that.

Usage example (from browser):
```javascript
function store(id, obj) {
    $.ajax({
        url: 'https://kv.jamhed.tk/key/bceea91e-d3a0-45fc-9a8a-5d8fdb6047f2/' + id,
        type: 'PUT',
        data: JSON.stringify(obj),
        contentType: "application/json",
        processData: false,
    });
}

store('what-ever-key', { data: "value" });
```

Setup
=====
```sh
# erlang must be installed (see docker image for mandatory erlang components)
make
```

Run
===
```sh
_rel/pubkv_release/bin/pubkv_release start
```

Setup and run with Docker
=========================
You can either manually create a docker image from supplied docker-file, or to use an image from docker hub repository:
```sh
mkdir Mnesia
docker run -td -v $(pwd)/Mnesia:/src/pubkv/_rel/pubkv_release/Mnesia -p 10080:10080 jamhed/pubkv
```

Live Demo
=========
[![asciicast](https://asciinema.org/a/br1h2k4a6alp1hs93w2xy2gay.png)](https://asciinema.org/a/br1h2k4a6alp1hs93w2xy2gay)


API
===

Generate UUID
-------------
```sh
KV=https://kv.jamhed.tk
UUID=`curl -s $KV/uuid`
```

Put
---
```sh
curl -X PUT -H "Content-Type: application/json"  -d '{"key": "json data"}' $KV/key/$UUID/some-data
curl -X PUT -H "Content-Type: application/json"  -d '{"key": "other data"}' $KV/key/$UUID/some-other-data
```
Returns: HTTP 200, ok. Keys will be created with default TTL=24*3600 seconds (one day).

Put with TTL
------------
```sh
curl -X PUT -d data $KV/key/$UUID/some-key?ttl=60
```
TTL = Time To Live in seconds. Key will be deleted after TTL will expire.

Persistent keys
---------------
```sh
curl -X PUT -d data $KV/key/$UUID/some-key?ttl=keep
```

Hidden keys
-----------
```sh
curl -X PUT -d data $KV/skey/$UUID/hidden-key
curl $KV/skey/$UUID/hidden-key
curl -X DELETE $KV/skey/$UUID/hidden-key
```
Hidden keys (skey) are stored as sha256 hashes, and are not visible
in keys list response to curl $KV/skey/$UUID.

List keys
----------
```sh
curl $KV/key/$UUID
```
Returns: HTTP 200, ["some-data", "some-other-data"] or []

Get
---
```sh
curl $KV/key/$UUID/some-data
```
Returns: HTTP 200, {"key": "json data"}
```sh
curl $KV/key/$UUID/some-missing-key
```
Returns: HTTP 404, not found

Delete
------
```sh
curl -X DELETE $KV/key/$UUID/some-data
curl -X DELETE $KV/key/$UUID
```
Returns: HTTP 200, ok 

Readonly Aliases
----------------
Create an UUID alias only for reading (write and delete take no effect)
```sh
ALIAS=`curl -s $KV/alias/$UUID`
curl $KV/key/$ALIAS/some-data
```

Replication
===========

You can run several copies of KV store, all of them synchronized. Here is an example with docker image:
```sh
docker run -td --name pubkv-slave-1 jamhed/pubkv slave
SLAVE_NODE=`docker exec pubkv-slave-1 info`
docker run -ti --name pubkv-master jmahed/pubkv master "'$SLAVE_NODE'"
```

After this two instances will be completely synchronized. If one goes down another will continue to work.
You can have more than one slave node: master "'$SLAVE_NODE_1','$SLAVE_NODE_2'", and so on.

Event API
=========

One can subscribe to UUID events delete and update, by using Server Side Events (SSE, EventSource)
```sh
curl $KV/watch/$UUID
# update some/key
# delete some/key
```

CORS Support
============

Each OPTIONS request returns response with headers set:
```text
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true
Access-Control-Allow-Methods: VALUE_OF(Access-Control-Request-Method)
Access-Control-Allow-Headers: VALUE_OF(Access-Control-Request-Headers)
```

Therefore allowing use from everywhere.

Closing words
=============

Public beta available at: https://kv.jamhed.tk

Source code: https://github.com/jamhed/pubkv

Docker image: https://hub.docker.com/r/jamhed/pubkv/

