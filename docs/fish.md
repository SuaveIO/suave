---
layout: default
---

Programming with fish (custom operator reference)
----------------------------------------

Functional programming tends to involve custom operators. An excessive number of custom operators makes for cryptic, illegible code, but a few well chosen ones allow logic to be more succinct and readable. We have already seen `>=>`, and happily suave does not use either ><<*> or <*)))>{

The other custom operators it declares are:

| Operator | Description |
| ---------|-------------|
|>=>       | Left-to-right Kleisli composition of monads, see Http.fsi
|<&#124;>  | Left-to-right Kleisli composition of web parts, see Http.fsi
|?         | Try find a value by key in a dictionary
|%%        | Search a list of key-value pairs and return the value (or None if not found)
|^^        | Search a list of key-value option pairs and return the value (or None if not found)
|?<-       | Assign a value to the key in the dictionary
