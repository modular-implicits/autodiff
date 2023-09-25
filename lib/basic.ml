open Imp.Data ;;
open Imp.Any;;
open Funcs;;


type 'a d = D of 'a * 'a;;

let constD x = D (x,0.);;

let idD x = D (x,1.);;

(*let ( + ) {N : Num} = N.( + )*)

exception Not_Implemented of string

let (><) {N : Num} (f : N.t -> N.t) (f' : N.t -> N.t) : (N.t d -> N.t d) = fun (D (a, a')) ->  D (f a, N.(f' a * a'))

let recip {M : Fractional} x = M.(one /. x)
let sqr {N : Num} x = N.(x * x)

implicit module Diff {F : Floating} : sig 
  include Eq with type t = F.t d
  include Num with type t := t
  include Fractional with type t := t
  include Floating with type t := t
end = struct 
  type t = F.t d

  (* Eq *)
  let ( = ) (D (x, x')) (D (y, y')) = x = y && x' = y'

  module FA : Any with type t = F.t = struct 
    type t = F.t
    let __any__ = ()
  end
  module FN = NumFunc {F} {FA}

  (* Num *)

  let zero = D (F.zero, F.zero)
  let one = D (F.one, F.zero)
  let of_int (x : int) = D (F.of_int x, F.zero)

  let ( + ) (D (x, x')) (D (y, y')) : t = D (F.(x + y),F.(x' + y'))
  let ( - ) (D (x, x')) (D (y, y')) : t = D (F.(x - y),F.(x' - y'))
  let ( * ) ((D (x, x')) : t) ((D (y, y')) : t) : t = D (F.(x * y), F.((x * y') + (x' * y)))
  let ( / ) (D (_, _)) (D (_, _)) : t = raise (Not_Implemented "not differentiable") 
  let (~-) = F.(~-) >< FN.of_int (-1)



  (* Fractional *)

  let of_fractional (x : float) : t = D (F.of_fractional x, F.zero)

  let recip' : t -> t = recip {F} >< FN.(~-) ((sqr {FN}) (recip {F}))
  let (/.) x y : t = x * (recip' y)

  (* Floating *)



  let pi = D (F.pi, F.zero)
  let sqrt = F.sqrt >< FN.(one /. (of_int 2 * F.sqrt))
  let exp = F.exp >< F.exp
  let log = F.log >< recip {F}
  let sin = F.sin >< F.cos
  let cos = F.cos >< FN.(~-) F.sin
  let asin = F.asin >< FN.(one /. (sqrt (one - sqr {F})))
  let acos = F.acos >< FN.(~- one /. (sqrt (one - sqr {F})))
  let atan : t -> t = F.atan >< FN.(one /. (one + sqr {F}))
  let sinh = F.sinh >< F.cosh
  let cosh = F.cosh >< F.sinh
end 