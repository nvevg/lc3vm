open! Base

let swap16 v = 
  let b16 = v land 0xFFFF in
  let lowb8 = (b16 land 0x00FF) lsl 8 in
  let hib8 = (b16 land 0xFF00) lsr 8 in
  lowb8 lor hib8
;;

let ( let* ) x f = 
  match x with
  | Ok v -> f v
  | Error e -> Error e
;;

let ( +^ ) x y = (x + y) land 0xffff

let bits word ~pos ~width = 
  let w16 = word land 0xFFFF in
  let mask = (1 lsl width) - 1 in
  (w16 lsr pos) land mask


let%test_unit "bits" =
  [%test_eq: int] (bits 0b1001 ~pos:0 ~width:4) 0b1001;
  [%test_eq: int] (bits 0b1101000000000000 ~pos:12 ~width:4) 0b1101;
;;

let sext16 n bit_count =
  let mask = (1 lsl bit_count) - 1 in
  let n = n land mask in
  let sign_bit = 1 lsl (bit_count - 1) in
  if n land sign_bit = 0 then n
  else n lor (0xFFFF land (lnot mask))
;;

let%test_unit "sext16" = 
  [%test_eq: int] (sext16 0b110000 6) 0b1111111111110000;
  [%test_eq: int] (sext16 0b1001 4) 0b1111111111111001;
  [%test_eq: int] (sext16 0b010 3) 0b0000000000000010;
;;

let int_to_bstr v = 
  let rec aux n acc = 
    if n = 0 then acc
    else
    let b = (n land 1) in
    aux (n lsr 1) ((Stdlib.string_of_int b) ^ acc)
  in
  if v = 0 then "0b0" else "0b" ^ (aux v "")
;;

let%test_unit "int_to_bstr" = 
  [%test_eq: string] (int_to_bstr 0b110000) "0b110000";
  [%test_eq: string] (int_to_bstr 0b10101) "0b10101";
  [%test_eq: string] (int_to_bstr 0b0) "0b0";
;;