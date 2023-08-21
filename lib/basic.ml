open Imp.Data ;;
open Imp.Data.Num;;
open Imp.Data.Fractional;;
open Imp.Data.Floating;;


type 'a d = D of 'a * 'a;;

let constD x = D (x,0.);;

let idD x = D (x,1.);;

(*let ( + ) {N : Num} = N.( + )*)

exception Not_Implemented of string

implicit module Diff {F : Floating} : sig 
  include Eq with type t = F.t d
  include Num with type t := t
  include Fractional with type t := t
  include Floating with type t := t
end = struct 
  type t = F.t d

  (* Eq *)
  let ( = ) (D (x, x')) (D (y, y')) = x = y && x' = y'

  (* Num *)

  let zero = D (F.zero, F.zero)
  let one = D (F.one, F.zero)
  let of_int (x : int) = D (F.of_int x, F.zero)

  let ( + ) (D (x, x')) (D (y, y')) : t = D (F.(x + y),F.(x' + y'))
  let ( - ) (D (x, x')) (D (y, y')) : t = D (F.(x - y),F.(x' - y'))
  let ( * ) ((D (x, x')) : t) ((D (y, y')) : t) : t = D (F.(x * y), F.((x * y') + (x' * y)))
  let ( / ) (D (x, x')) (D (y, y')) : t = raise (Not_Implemented "not differentiable") (* D (F.(x / y), F.((x' * y) - ((x * y') / (y * y)))) *)
  let (~-) (D (x, x')) : t = D (F.(~- x), F.(~- x'))


  (* Fractional *)

  let of_fractional (x : float) : t = D (F.of_fractional x, F.zero)

  let recip (D (x, x')) : t = D (F.(one /. x), F.(x' / (x * x)))

  let (/.) x y : t = x * (recip y)

  (* Floating *)

  let pi = D (F.pi, F.zero)
  let sqrt (D (x, x')) : t = D (F.sqrt x,  F.(x' /. ((of_int 2) * sqrt x)))
  let exp (D (x, x')) : t = D (F.exp x, F.(x' * (exp x)))
  let log (D (x, x')) : t = D (F.log x, F.(x' /. x))
  let sin (D (x, x')) : t = D (F.sin x, F.(x' * (cos x)))
  let cos (D (x, x')) : t = D (F.cos x, F.(~-) (F.(x' * (sin x))))
  let asin (D (x, x')) : t = D (F.asin x, F.(x' /. (sqrt (F.one - (x * x)))))
  let acos (D (x, x')) : t = D (F.acos x, F.(~-) F.(x'  /. (sqrt (one - (x * x)))))
  let atan (D (x, x')) : t = D (F.atan x, F.(x' /. (one + (x * x))))
  let sinh (D (x, x')) : t = D (F.sinh x, F.(x' * (cosh x)))
  let cosh (D (x, x')) : t = D (F.cosh x, F.(x' * sinh x))
end 


(*
implicit module DAllNums {A : Any} {F : Floating} : sig 
  include Num with type t := F.t d
  include Fractional with type t := t
  include Floating with type t := t
end = struct*)