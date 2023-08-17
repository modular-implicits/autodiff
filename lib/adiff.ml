open Imp;;


type 'a d = D of 'a * 'a;;

constD x = D (x,0);;

idD x = D (x,1);;

implicit module DAllNums {F : Floating} : 
  include Eq with type t = F.t d
  include Num with type t := t
  include Fractional with type t := t
end = struct 
  type t = F.t d

  (* Eq *)
  let ( = ) (D (x1, x2)) (D (y1, y2)) = x1 = y1 && x2 = y2

  (* Num *)

  let zero = D (F.zero, F.zero)
  let one = D (F.one, F.zero)
  let of_int (x : int) = D (F.of_int x, F.zero)

  let ( + ) (D (x1, x2)) (D (y1, y2)) = D (x1 + y1, x2 + y2)
  let ( - ) (D (x1, x2)) (D (y1, y2)) = D (x1 - y1, x2 - y2)
  let ( * ) (D (x1, x2)) (D (y1, y2)) = D (x1 * y1, x1 * y2 + x2 * y1)
  let ( / ) (D (x1, x2)) (D (y1, y2)) = D (x1 / y1, (x2 * y1 - x1 * y2) / (y1 * y1))
  let (~-) (D (x1, x2)) = D (F.(~-) x1, F.(~-) x2)


  (* Fractional *)

  let of_fractional (x : F.t) = D (F.of_fractional x, F.zero)
  let fdiv (D (x1, x2)) = D (F.fdiv (F.one) x1, F.(~-) (x2 / (x1 * x1)))

  (* Floating *)





end 