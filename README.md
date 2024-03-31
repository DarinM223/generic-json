generic-json
============

Automatic JSON serialization and deserialization from type indexed values.

See the `example/` folder for an example for how to use the library. The type indexed value
representations in `example.sml` were generated using [smlgen](https://github.com/DarinM223/smlgen).

Building
--------

To build the example, first run `smlpkg sync` in the project folder, then go into the example folder and run `mlton example.mlb` to build with MLton or `sml -m example.cm` to run with SML/NJ.