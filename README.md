Public KV
=========

Setup
=====
```
$ make
```

Run
===

```
$ _rel/pubkv_release/bin/pubkv_release console
```

Use
===

```
$ KV = http://localhost:10080
$ UUID = `curl $KV/uuid`
$ curl -d "some data" $KV/key/$UUID/data
$ curl $KV/key/$UUID/data
```
