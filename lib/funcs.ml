open Imp.Control;;
open Imp.Data.Num;;
open Imp.Data.Fractional;;
open Imp.Data.Floating;;
open Imp.Data;;
open Imp.Any;;



implicit module NumFunc {B : Floating} {A : Any} : sig  
include Num with type t = A.t -> B.t
include Fractional with type t := t 
include Floating with type t := t end = struct 
  type t = A.t -> B.t

  (* Num *)

  let zero = fun _ -> B.zero
  let one = fun _ -> B.one
  let ( + ) : t -> t -> t = liftA2 B.( + )
  let ( * ) : t -> t -> t = liftA2 B.( * )
  let ( - ) : t -> t -> t = liftA2 B.( - )
  let ( / ) : t -> t -> t = liftA2 B.( / )
  let ( ~- ) : t -> t = fmap B.( ~- )
  let of_int (x : int) : t = fun _ -> B.of_int x
  (* let abs = fmap B.abs *)

  (* Fractional *)

  let of_fractional (x : float) : t = fun _ -> B.of_fractional x
  let (/.) : t -> t -> t = liftA2 B.(/.)

  (* Floating *)
  let pi : t = pure B.pi
  let exp : t -> t = fmap B.exp
  let log : t -> t = fmap B.log
  let sqrt : t -> t = fmap B.sqrt
  let sin : t -> t = fmap B.sin
  let cos : t -> t = fmap B.cos
  let asin : t -> t = fmap B.asin
  let acos : t -> t = fmap B.acos
  let atan : t -> t = fmap B.atan
  let sinh : t -> t = fmap B.sinh
  let cosh : t -> t = fmap B.cosh
end 




