type t = {
  r0 : int; r1 : int; r2 : int; r3 : int;
  r4 : int; r5 : int; r6 : int; r7 : int;
}

let make () =
  { r0 = 0; r1 = 0; r2 = 0; r3 = 0; r4 = 0; r5 = 0; r6 = 0; r7 = 0; }
;;

type register = 
  | R_R0
  | R_R1
  | R_R2
  | R_R3
  | R_R4
  | R_R5
  | R_R6
  | R_R7
;;

let register_from_int n = 
  match n with
  | 0 -> Ok R_R0
  | 1 -> Ok R_R1
  | 2 -> Ok R_R2
  | 3 -> Ok R_R3
  | 4 -> Ok R_R4
  | 5 -> Ok R_R5
  | 6 -> Ok R_R6
  | 7 -> Ok R_R7
  | r -> Error (`UnknownRegister r)
;;

let name = function
  | R_R0 -> "R0"
  | R_R1 -> "R1"
  | R_R2 -> "R2"
  | R_R3 -> "R3"
  | R_R4 -> "R4"
  | R_R5 -> "R5"
  | R_R6 -> "R6"
  | R_R7 -> "R7"
;;

let set ~register ~value registers =
  match register with
  | R_R0 -> { registers with r0 = value }
  | R_R1 -> { registers with r1 = value }
  | R_R2 -> { registers with r2 = value }
  | R_R3 -> { registers with r3 = value }
  | R_R4 -> { registers with r4 = value }
  | R_R5 -> { registers with r5 = value }
  | R_R6 -> { registers with r6 = value }
  | R_R7 -> { registers with r7 = value }
;;

let get ~register registers = 
  match register with
  | R_R0 -> registers.r0
  | R_R1 -> registers.r1
  | R_R2 -> registers.r2
  | R_R3 -> registers.r3
  | R_R4 -> registers.r4
  | R_R5 -> registers.r5
  | R_R6 -> registers.r6
  | R_R7 -> registers.r7
;;