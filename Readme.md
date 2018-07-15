# elm-barcodes

A library for rendering barcodes and handling text
input from barcode scanners.

Barcode scanners
‘type’ much faster than people, and elm has some issues with fast updates to
the `value` property of a text input (https://github.com/etaque/elm-form/issues/54).
The `<input>` manged by the `Barcode.Input` component does not have its `value`
property updated to match a value in the component state. It is, however,
possible to clear the input. This is accomplished by removing the original
`<input>` node and inserting a new one.

To compile the example app, run the following command from the main project dir:

    elm make example/Main.elm --output app.html