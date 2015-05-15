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

## Stores

### `mixr_mem_store`

"On memory" storage

### `mixr_ets_store`

Store data on an ETS file

Parameters :

* `file` :: `string()` : Name of the ETS file (default: `~/.data-<node()>.mixr`)
* `tid` :: `atom()` : Name of the ETS file (default: `mixr`)

## TODO

* LeoFS Storage
* AWS Storage
* SQL storage
* Redis Storage

## Licence

Mixr is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014, 2015 Grégoire Lejeune

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
