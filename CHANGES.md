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
