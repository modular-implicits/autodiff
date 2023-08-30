open Imp.Data ;;
open Imp.Data.Num;;
open Imp.Data.Fractional;;
open Imp.Data.Floating;;


type 'a d = D of 'a * 'a;;

let constD x = D (x,0.);;

let idD x = D (x,1.);;

(*let ( + ) {N : Num} = N.( + )*)

exception Not_Implemented of string

let (><) {N : Num} (f : N.t -> N.t) (f' : N.t -> N.t) : (N.t d -> N.t d) = fun (D (a, a')) ->  D (f a, N.(f' a * a'))

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

  let recip : t -> t = (fun x -> F.(one /. x)) >< fun x -> F.(~- one / (x * x))
  let (/.) x y : t = x * (recip y)

  (* Floating *)

  let pi = D (F.pi, F.zero)
  let sqrt = F.sqrt >< fun x -> F.(one /. (of_int 2 * sqrt x))
  let exp = F.exp >< F.exp
  let log = F.log >< fun x -> F.(one /. x)
  let sin = F.sin >< F.cos
  let cos = F.cos >< fun x -> F.(~-) (F.sin x)
  let asin = F.asin >< fun x -> F.(one /. (sqrt (one - (x * x))))
  let acos = F.acos >< fun x -> F.(~- one /. (sqrt (one - (x * x))))
  let atan = F.atan >< fun x -> F.(one /. (one + (x * x)))
  let sinh = F.sinh >< F.cosh
  let cosh = F.cosh >< F.sinh
end 