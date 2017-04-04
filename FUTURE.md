Future plans
============

Put here anything what comes to your mind regarding what features could be
added/removed/improved/changed, etc... etc... etc...


Metadata
========

Something like:

```sh
curl -s $KV/key/$UUID?meta |jq .
```

would give the metadata of the UUID key itself, e.g.:
- updated   (last time the object was updated)

```sh
curl -s $KV/key/$UUID/abc/?meta |jq .
```

would give us the metadata for the "abc" key, e.g.:
- updated   (last time the object was updated)
- checksum  (arbitrary checksum given by the client)


Alternative way of the key accessing
====================================

Obtaining the metadata of some key
```sh
$KV/key/$UUID/abc/meta/checksum |jq .
$KV/key/$UUID/abc/meta/size |jq .
```

Obtaining the contents of the key
```sh
$KV/key/$UUID/abc/gef |jq .
```

