open Imp.Any;;
open Imp.Control;;





module type AdditiveGroup = sig
  type t
  val zero : t
  val ( + ) : t -> t -> t
  val negate : t -> t
end

module type Vector = sig 
include AdditiveGroup
  type s
  val ( >>> ) : s -> t -> t
end

let (>>>) {V : Vector} = V.( >>> )

implicit module AdditiveGroupFun {V : AdditiveGroup} {A : Any} : AdditiveGroup with type t = A.t -> V.t = struct
  type t = A.t -> V.t
  let zero : t = pure V.zero
  let ( + ) : t -> t -> t = liftA2 V.( + )
  let negate : t -> t = fmap V.negate
end

implicit module VectorFun {V : Vector} {A : Any} : Vector with type t = A.t -> V.t and type s = V.s = struct
  type t = A.t -> V.t
  type s = V.s
  (* This is duplicated annoyingly *)
  let zero : t = pure V.zero
  let ( + ) : t -> t -> t = liftA2 V.( + )
  let negate : t -> t = fmap V.negate

  let ( >>> ) (s : V.s) : t -> t = fmap (fun x -> s >>> x)
end

type ('u, 'v) linMap = LMap of ('u -> 'v)

