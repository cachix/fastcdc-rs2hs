# fastcdc-rs2hs

A Haskell wrapper around [fastcdc-rs](https://github.com/nlfiedler/fastcdc-rs).

## Building

```
direnv allow .
cargo cbuild -r --prefix=$PWD/target
cargo cinstall -r --prefix=$PWD/target
cabal build all
```
