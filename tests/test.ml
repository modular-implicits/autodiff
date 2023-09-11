open Adiff.Basic;;
open Imp.Data;;
open Imp.Data.Num;;
open Imp.Data.Fractional;;
open Imp.Data.Floating;;



let alg {N : Floating} (x : N.t) : N.t = N.exp x;;

let alg2 {N : Floating} (x : N.t) : N.t = N.(
          sqrt ((of_int 3) * (sin x))
)

let print_diff (D (x, x')) = print_endline ("x = " ^ (string_of_float x) ^ ", x' = " ^ (string_of_float x'))

let () = 
    let x = alg2 (idD (2.)) in 
      print_diff x;;

