Public KV
=========

Suppose you want to store something from script running in browser, and then retrieve
the value from somewhere else. This is a primitive key-value store exactly for that.

Setup
=====
```
$ make
```

Run
===
```
$ _rel/pubkv_release/bin/pubkv_release start
```

Use
===
```
$ KV = http://kv.jamhed.tk | http://localhost:10080
$ UUID = `curl $KV/uuid`
```

Put
---
```
$ curl -X PUT -H "Content-Type: application/json"  -d '{"key": "json data"}' $KV/key/$UUID/some-data
$ curl -X PUT -H "Content-Type: application/json"  -d '{"key": "other data"}' $KV/key/$UUID/some-other-data
Returns: HTTP 200, ok
```

List keys
----------
```
$ curl $KV/key/$UUID
Returns: HTTP 200, ["some-data", "some-other-data"] or []
```

Get
---
```
$ curl $KV/key/$UUID/some-data
Returns: HTTP 200, {"key": "json data"}
```
```
$ curl $KV/key/$UUID/some-missing-key
Returns: HTTP 404, not found
```

Delete
------
```
$ curl -X DELETE $KV/key/$UUID/some-data
$ curl -X DELETE $KV/key/$UUID
Returns: HTTP 200, ok 
```

Readme Alias
------------
Create an UUID alias only for reading (write and delete take no effect)
```
$ ALIAS = `curl $KV/alias/$UUID`
$ curl $KV/key/$ALIAS/some-data
```

Closing words
=============

Public beta available at: http://kv.jamhed.tk
