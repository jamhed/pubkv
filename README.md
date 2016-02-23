Public KV
=========

Suppose you want to store something from script running in browser, and then retrieve
the value from somewhere else. This is a primitive key-value store exactly for that. Keys must not contain '/'.

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
make
```

Run
===
```sh
_rel/pubkv_release/bin/pubkv_release start
```

Run (docker version)
====================
```sh
mkdir Mnesia
docker run -td -v $(pwd)/Mnesia:/src/pubkv/_rel/pubkv_release/Mnesia -p 10080:10080 jamhed/pubkv:latest
```

Use
===
```sh
KV=https://kv.jamhed.tk
UUID=`curl $KV/uuid`
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

List keys
----------
```sh
curl $KV/key/$UUID
```
Returns: HTTP 200, ["some-data", "some-other-data"] or []

Get
---
```sh
$ curl $KV/key/$UUID/some-data
```
Returns: HTTP 200, {"key": "json data"}
```sh
$ curl $KV/key/$UUID/some-missing-key
```
Returns: HTTP 404, not found

Delete
------
```sh
$ curl -X DELETE $KV/key/$UUID/some-data
$ curl -X DELETE $KV/key/$UUID
```
Returns: HTTP 200, ok 

Readonly Aliases
----------------
Create an UUID alias only for reading (write and delete take no effect)
```sh
$ ALIAS = `curl $KV/alias/$UUID`
$ curl $KV/key/$ALIAS/some-data
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
