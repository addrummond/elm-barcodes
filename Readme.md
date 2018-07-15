# elm-barcodes

A library for rendering barcodes and handling text
input from barcode scanners.

Barcode scanners ‘type’ much faster than people. Elm has some issues with fast
updates to the `value` property of a text input
(https://github.com/etaque/elm-form/issues/54). The text input component
provided by `Barcode.Input` does not update the `value` property of the
`<input>` node, but instead removes the original input node and inserts a new
one (setting `defaultValue` appropriately). The contents of the `<input>` can
be accessed only when a message is triggered in response to a key press.

To compile the example app, run the following command from the main project dir:

    elm make example/Main.elm --output app.html