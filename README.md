## Haskell bindings to [UnQLite](https://unqlite.org/)

  Learning Haskell FFI by making bindings to C library.
  
  Experimental. Incomplete.

## TODO:

High priority

* [x] Open DB connection
* [x] Close DB connection
* [x] Store key\value
* [x] Fetch key\value
* [x] DB init config (access modes)
* [x] Append value
* [x] Delete key
* [x] Transactions begin\commit\rollback
* [ ] Cursor interface
* [ ] Document Store (JSON via Jx9). A very complex and very promising feature.

Low priority

* [ ] Configure the underlying Key/Value (KV) storage engine.
* [x] DB config (config database handle obtained by a prior successful call to unqlite_open())
* [ ] Formatted store\append
* [ ] Callback fetching
