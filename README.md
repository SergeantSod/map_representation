# map_representation

An example for how to use shapeless to derive type class instances for case classes automatically.
Inspired by http://underscore.io/books/shapeless-guide/

This provides a type class `MapRepresentation`, which allows converting instances of case classes to corresponding instances of  `Map[String, Any]`.
Conversion happens recursively and the collection types `Set`, `Seq` and `Map` are supported for members.

This an example to explore shapeless, and is probably not particularly useful per se, but might come in handy when interacting with JRuby.
