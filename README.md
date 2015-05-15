# Mixr

memcached rewriten in pure Erlang

# Support

This version, support :

* the [binary protocol](https://code.google.com/p/memcached/wiki/BinaryProtocolRevamped).
* a custom REST API.

## Memcached commands supported :

* SET
* ADD
* REPLACE
* SETQ
* ADDQ
* REPLACEQ
* GET
* GETQ
* GETK
* GETKQ
* DELETE
* DELETEQ
* APPEND
* APPENDQ
* PREPEND
* PREPENDQ

## REST API :

**GET /count**

**GET /d/:key**

**GET /d/:key/cas**

**GET /d/:key/expire**

**POST /d/:key[/expire/:seconds]**

**PUT /d/:key/cas/:cas**

**DELETE /d/:key/cas/:cas**

