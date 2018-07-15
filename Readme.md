# elm-barcodes

A library for rendering barcodes (currently code 39 only) and handling text
input from barcode scanners. The latter is not trivial, since barcode scanners
‘type’ much faster than people, and elm has some issues with fast updates to
the `value` property of a text input:

    https://github.com/etaque/elm-form/issues/54