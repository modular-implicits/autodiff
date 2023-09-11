Library for forward differentation using the Num module available in `imp`

As an example you can write arbitrary functions on numbers:

```ocaml
let alg {N : Floating} (x : N.t) : N.t = N.exp x;;

(* You can then use this with the d data-type to forward differentiate *)

let (value, diff) = (alg (idD (2.)))
```
