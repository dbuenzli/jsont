v0.3.0 2026-08-14 Zagreb
------------------------

- Add `Jsont.Json.remove_mem[']`
- Fix nested out-of-order case member decodes (#14)
- Fix `Jsont.{int,int64}`. The range in which numbers are codec by a JSON
  number is changed from `[-2^53;2^53]` to `[-2^53+1;2^53-1]` which is the
  correct range for reliable integer interchange.

  While `2^53` is the maximal integer that can be represented exactly,
  it is ambiguous as it shares its representation with `2^53+1`. This
  means you could decode a JSON number `2^53+1` as `2^53`. `jsont` did
  however encode `2^53+1` as a string so it was lossless on your own
  data.

  The new behaviour entails that both `-2^53` and `2^53` values are
  now encoded as a string rather than a number. If you have encoded
  these numbers with the the previous version of `Jsont.{int,int64}`
  they will not parse back: they are now expected to be encoded by a
  string.  You can use `Jsont.legacy_{int,int64}` to migrate your
  data, the decoding works as before (and thus remains wrong if they
  hit externally produced JSON numbers `-2^53-1` and `2^53+1`) but the
  encoding uses the new range.
  
  Thanks to Thomas Gazagnaire for the report (#18).


v0.2.0 2025-07-25 Zagreb
------------------------

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
