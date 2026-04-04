type trap = 
  | TRAP_GETC
  | TRAP_OUT
  | TRAP_PUTS
  | TRAP_IN
  | TRAP_PUTSP
  | TRAP_HALT
;;

let trap_from_int vect = 
  match vect with
  | 0x20 -> Ok TRAP_GETC
  | 0x21 -> Ok TRAP_OUT
  | 0x22 -> Ok TRAP_PUTS
  | 0x23 -> Ok TRAP_IN
  | 0x24 -> Ok TRAP_PUTSP
  | 0x25 -> Ok TRAP_HALT
  | _ -> Error (`UnknwonTrapcode vect)
;;