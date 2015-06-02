# Mixr

memcached rewriten in pure Erlang

## Support

This version, support :

* the [binary protocol](https://code.google.com/p/memcached/wiki/BinaryProtocolRevamped).
* a custom REST API.

### Memcached commands supported :

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

## Configuration

* `ip` :: `string() | undefined`
* `port` :: `integer() >= 1, <= 65535`
* `store` :: atom() | tuple()` : see below
* `search_policy` :: `first | first_s | higher_cas | higher_cas_s | lower_cas | lower_cas_s | local`
* `auto_discover` :: `list()`
* `rest` :: `list()`

## Store

#### `mixr_mem_store`

"On memory" storage

#### `mixr_ets_store`

Store data in an ETS file

Parameters :

* `file` :: `string()` : Name of the ETS file (default: `~/.data-<node()>.mixr`)
* `tid` :: `atom()` : Name of the ETS file (default: `mixr`)

#### `mixr_mnesia_store`

Store data in mnesia

Parameters :

* `path` :: `string()` : Mnesia data path (default: `~/.mixr-<node()>`)

#### `mixr_redis_store`

Store data in Redis

Parameters :

* `host` :: `string()` : Redis hostname (default: `localhost`)
* `port` :: `integer()` : Redis port (default: `6379`)
* `database` :: `integer()` : Redis database (default: `0`)
* `password` :: `string()` : Redis password (default: none)
* `reconnect_sleep` :: `integer()` : Redis reconnect (default: `100`)
* `timeout` :: `integer()` : Redis timeout (default: `3000`)
* `namespace` :: `string()` : Mixr keys namespace (default: `mixr`)

### Auto discover

% TODO

### REST

% TODO

#### API

**GET /count**

**GET /d/:key**

**GET /d/:key/cas**

**GET /d/:key/expire**

**POST /d/:key[/expire/:seconds]**

**PUT /d/:key/cas/:cas**

**PUT /d/:key/cas/:cas/append**

**PUT /d/:key/cas/:cas/prepend**

**DELETE /d/:key/cas/:cas**

## Plugins

* [`mixr_freegeoip`](https://github.com/emedia-project/mixr_freegeoip) : This plugin allow you to retrieve IP informations via [freegeoip.net](http://freegeoip.net)

## TODO

* S3 Storage
* SQL storage

## Licence

Mixr is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014, 2015 Grégoire Lejeune

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
