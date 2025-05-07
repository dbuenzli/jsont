
- Fix `Jsont_bytesrw.{encode,encode'}`. Do not write the `eod` slice if
  `eod:false` is specified. Thanks to Benjamin Nguyen-Van-Yen for
  the report and the fix (#8).
- Fix `Jsont.zero` failing encodes rather than encoding `null` as
  advertised. Thanks to Adrián Montesinos González for the report (#6).
- Add `Jsont.Error.expected` to help format error messages.
- Add `Jsont.with_doc` to update kind and doc strings of existing JSON
  types.
- Add `Jsont.Object.Case.{tag,map_tag}` to access a case and case map tags.
- Fix `META` file. Really export all requires and
  remove uneeded `bytesrw` dependency from `jsont` library.

v0.1.1 2024-12-06 La Forclaz (VS)
---------------------------------

- `Jsont.Object.Mems.map` make encoding and decoding optional. Like
   in every other map.
- `Jsont.Array.map` make encoding and decoding optional. Like
   in every other map.
- `Jsont_bytesrw.encode` change the default buffer size
  to match the one hinted by the writer rather than
  `Bytesrw.Bytes.Slice.io_buffer_size`.
- `jsont.{bytesrw,brr}` export all requires.
- `jsont` tool remove spurious dependency on `b0.std` (#2).

v0.1.0 2024-11-29 Zagreb
------------------------

First release.

Supported by a grant from the OCaml Software Foundation.
