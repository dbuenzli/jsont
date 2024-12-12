Jsont – Declarative JSON data manipulation for OCaml
====================================================

Jsont is an OCaml library for declarative JSON data manipulation. It
provides:

- Combinators for describing JSON data using the OCaml values of your
  choice. The descriptions can be used by generic functions to
  decode, encode, query and update JSON data without having to
  construct a generic JSON representation.
- A JSON codec with optional text location tracking and layout
  preservation. The codec is compatible with effect-based concurrency.

The descriptions are independent from the codec and can be used by
third-party processors or codecs.

Jsont is distributed under the ISC license. It has no dependencies.
The codec is optional and depends on the [`bytesrw`] library. The JavaScript
support is optional and depends on the [`brr`] library.

Homepage: <https://erratique.ch/software/jsont/>

[`bytesrw`]: https://erratique.ch/software/bytesrw
[`brr`]: https://erratique.ch/software/brr

## Installation

Jsont can be installed with `opam`: 

    opam install jsont 
    opam install jsont bytesrw   # For the optional codec support
    opam install jsont brr       # For the optional JavaScript support
    opam install jsont bytesrw cmdliner  # For the jsont tool

## Documentation

The documentation can be consulted [online] or via `odig doc jsont`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker. 

[online]: https://erratique.ch/software/jsont/doc
[OCaml forum]: https://discuss.ocaml.org/

## Examples

A few examples can be found in the [documentation][online] and in the
[test](test/) directory. The [`test/topojson.ml`],
[`test/geojson.ml`], [`test/json_rpc.ml`], show use of the library on 
concrete JSON data formats.

[`test/topojson.ml`]: test/topojson.ml
[`test/geojson.ml`]: test/geojson.ml
[`test/json_rpc.ml`]: test/json_rpc.ml

## Paper & technique

If you want to understand the *finally tagged* technique used by the
library, the [`paper/soup.ml`] source implements the abridged version
of the underlying data type used in [the paper].

[the paper]: paper/
[`paper/soup.ml`]: paper/soup.ml

## Acknowledgments 

A grant from the [OCaml Software Foundation] helped to bring the first
public release of `jsont`.

[OCaml Software Foundation]: http://ocaml-sf.org/
