open Utils

type register_or_value = 
  | Register of Registers.register
  | Value of int

type binary_op = { dr: Registers.register; sr1: Registers.register; sr2: register_or_value }

type unary_op = { dr: Registers.register; sr: Registers.register }

type br_op = { n: bool; z: bool; p: bool; pc_offset: int }

type jmp_op = { base_r: Registers.register }

type ret_op = Empty

type jsr_op = { pc_offset: int }

type jsrr_op = { base_r: Registers.register; }

type ld_op = { dr: Registers.register; pc_offset: int }

type ldi_op = { dr: Registers.register; pc_offset: int }

type ldr_op = { dr: Registers.register; base_r: Registers.register; offset: int }

type lea_op = { dr: Registers.register; pc_offset: int }

type rti_op = Empty 

type st_op = { sr: Registers.register; pc_offset: int }

type sti_op = { sr: Registers.register; pc_offset: int }

type str_op = { sr: Registers.register; base_r: Registers.register; pc_offset: int }

type trap_op = { vect: int }

type opcode = 
  | OP_AND of binary_op
  | OP_ADD of binary_op
  | OP_NOT of unary_op
  | OP_BR of br_op
  | OP_JMP of jmp_op
  | OP_RET of ret_op
  | OP_JSR of jsr_op
  | OP_JSRR of jsrr_op
  | OP_LD of ld_op
  | OP_LDI of ldi_op
  | OP_LDR of ldr_op
  | OP_LEA of lea_op
  | OP_RTI of rti_op
  | OP_ST of st_op
  | OP_STI of sti_op
  | OP_STR of str_op
  | OP_TRAP of trap_op

let parse_binary_op opw = 
  let* dr = Registers.register_from_int (Utils.bits opw ~pos:9 ~width:3) in
  let* sr1 = Registers.register_from_int (Utils.bits opw ~pos:6 ~width:3) in
  let b5_flag = Utils.bits opw ~pos:5 ~width:1 in
  if b5_flag = 0 then
    let* sr2 = Registers.register_from_int (Utils.bits opw ~pos:0 ~width:3) in
    Ok { dr; sr1; sr2 = Register sr2; }
  else
    let value_bits = Utils.bits opw ~pos:0 ~width:5 in
    Ok { dr; sr1; sr2 = Value (Utils.sext16 value_bits 5) }
  ;;

  let parse_unary_op opw = 
    let filler = Utils.bits opw ~pos:0 ~width:6 in
    match filler with
    | 0b111111 -> 
          let* dr = Registers.register_from_int (Utils.bits ~pos:9 ~width:3 opw) in
          let* sr = Registers.register_from_int (Utils.bits ~pos:6 ~width:3 opw) in
          Ok { dr; sr }
    | _ -> Error (`InvalidOpcode opw)
  ;;

let parse_add opw = parse_binary_op opw |> Result.map (fun c -> OP_ADD c)

let parse_and opw = parse_binary_op opw |> Result.map (fun c -> OP_AND c)

let parse_not opw = parse_unary_op opw |> Result.map (fun c -> OP_NOT c)

let parse_br opw = 
  let pc_offset = Utils.bits opw ~pos:0 ~width:9 in
  let n_enabled = Utils.bits opw ~pos:11 ~width:1 = 1 in
  let z_enabled = Utils.bits opw ~pos:10 ~width:1 = 1 in
  let p_enabled = Utils.bits opw ~pos:9 ~width:1 = 1 in 
  Ok (OP_BR { n = n_enabled; z = z_enabled; p = p_enabled; pc_offset = (Utils.sext16 pc_offset 9) })
;;

let parse_jmp_ret opw = 
  let filler1 = Utils.bits opw ~pos:9 ~width:3 in
  let filler2 = Utils.bits opw ~pos:0 ~width:5 in
  if (filler1 != 0b000 || filler2 != 0b000000) then Error (`InvalidOpcode opw)
  else
    let* register = Registers.register_from_int (Utils.bits opw ~pos:6 ~width:3) in
    match register with
    | Registers.R_R7 -> Ok (OP_RET Empty)
    | _ -> Ok (OP_JMP { base_r = register })
;;

let parse_jsr_jsrr opw = 
  let b11 = Utils.bits opw ~pos:11 ~width:1 in
  if b11 = 1 then 
    let pc_offset = Utils.bits opw ~pos:0 ~width:11 in
    Ok (OP_JSR { pc_offset = Utils.sext16 pc_offset 11 })
  else
    let* base_r = Registers.register_from_int (Utils.bits opw ~pos:6 ~width:3) in
    Ok (OP_JSRR { base_r })
;;

let parse_ld opw = 
  let pc_offset = Utils.bits opw ~pos:0 ~width:9 in
  let* register = Registers.register_from_int (Utils.bits opw ~pos:9 ~width:3) in
  Ok (OP_LD { dr = register; pc_offset = (Utils.sext16 pc_offset 10) })
;;

let parse_ldi opw = 
  let pc_offset = Utils.bits opw ~pos:0 ~width:9 in
  let* register = Registers.register_from_int (Utils.bits opw ~pos:9 ~width:3) in
  Ok (OP_LDI { dr = register; pc_offset = (Utils.sext16 pc_offset 9) })
;;

let parse_ldr opw = 
  let* dr = Registers.register_from_int (Utils.bits opw ~pos:9 ~width:3) in
  let* base_r = Registers.register_from_int (Utils.bits opw ~pos:6 ~width:3) in
  let pc_offset = Utils.bits opw ~pos:0 ~width:6 in
  Ok (OP_LDR { dr; base_r; offset = Utils.sext16 pc_offset 6} )
;;

let parse_lea opw = 
  let* dr = Registers.register_from_int (Utils.bits opw ~pos:9 ~width:3) in
  let pc_offset = Utils.bits opw ~pos:0 ~width:9 in
  Ok (OP_LEA { dr; pc_offset })
;;

let parse_rti opw = 
  let filler = Utils.bits opw ~pos:0 ~width:12 in
  if filler != 0b000000000000 then Error (`InvalidOpcode opw)
  else Ok (OP_RTI Empty)
;;

let parse_st opw = 
  let* sr = Registers.register_from_int (Utils.bits opw ~pos:9 ~width:3) in
  let offset = Utils.bits opw ~pos:0 ~width:9 in
  Ok (OP_ST { sr; pc_offset = Utils.sext16 offset 9; })
;;

let parse_sti opw = 
  let* sr = Registers.register_from_int (Utils.bits opw ~pos:9 ~width:3) in
  let offset = Utils.bits opw ~pos:0 ~width:9 in
  Ok (OP_STI { sr; pc_offset = Utils.sext16 offset 9; })
;;

let parse_str opw = 
  let* sr = Registers.register_from_int (Utils.bits opw ~pos:9 ~width:3) in
  let* base_r = Registers.register_from_int (Utils.bits opw ~pos:6 ~width:3) in
  let offset = Utils.bits opw ~pos:0 ~width:6 in
  Ok (OP_STR { sr; base_r; pc_offset = Utils.sext16 offset 6; })
;;

let parse_trap opw = 
  let trapvect = Utils.bits opw ~pos:0 ~width:8 in
  Ok (OP_TRAP { vect = trapvect })
;;

let parse_opcode opw =
  let opcode = Utils.bits opw ~pos:12 ~width:4 in
  match opcode with 
  | 0b0001 -> parse_add opw
  | 0b0101 -> parse_and opw
  | 0b0000 -> parse_br opw
  | 0b1100 -> parse_jmp_ret opw
  | 0b0100 -> parse_jsr_jsrr opw
  | 0b0010 -> parse_ld opw
  | 0b1010 -> parse_ldi opw
  | 0b0110 -> parse_ldr opw
  | 0b1110 -> parse_lea opw
  | 0b1001 -> parse_not opw
  | 0b1000 -> parse_rti opw
  | 0b0011 -> parse_st opw
  | 0b1011 -> parse_sti opw
  | 0b0111 -> parse_str opw
  | 0b1111 -> parse_trap opw
  | 0b1101 -> Error `UnusedOpcode
  | _ -> Error (`UnknownOpcode opcode)
;;

let register_or_value_to_string = function 
  | Register r -> Registers.name r
  | Value v -> Printf.sprintf "%#X" v

let opcode_to_string code = 
  match code with
  | OP_ADD op -> Printf.sprintf "ADD %s, %s, %s" (Registers.name op.dr) (Registers.name op.sr1) (register_or_value_to_string op.sr2)
  | OP_AND op -> Printf.sprintf "AND %s, %s, %s" (Registers.name op.dr) (Registers.name op.sr1) (register_or_value_to_string op.sr2)
  | OP_NOT op -> Printf.sprintf "NOT %s, %s" (Registers.name op.dr) (Registers.name op.sr)
  | OP_BR op -> 
      let suffix = (if op.n then "n" else "") ^ (if op.z then "z" else "") ^ (if op.n then "p" else "") in
      Printf.sprintf ("BR%s %#X") suffix op.pc_offset
  | OP_JMP op -> Printf.sprintf "JMP %s" (Registers.name op.base_r)
  | OP_RET _ -> "RET"
  | OP_JSR op -> Printf.sprintf "JSR %#X" op.pc_offset
  | OP_JSRR op -> Printf.sprintf "JSRR %s" (Registers.name op.base_r)
  | OP_LD op -> Printf.sprintf "LD %s, %#X" (Registers.name op.dr) op.pc_offset
  | OP_LDI op -> Printf.sprintf "LDI %s, %#X" (Registers.name op.dr) op.pc_offset
  | OP_LDR op -> Printf.sprintf "LDR %s, %s, %#X" (Registers.name op.dr) (Registers.name op.base_r) op.offset
  | OP_LEA op -> Printf.sprintf "LEA %s, %#X" (Registers.name op.dr) op.pc_offset
  | OP_RTI _ -> "RTI"
  | OP_ST op -> Printf.sprintf "ST %s, %#X" (Registers.name op.sr) op.pc_offset 
  | OP_STI op -> Printf.sprintf "STI %s, %#X" (Registers.name op.sr) op.pc_offset 
  | OP_STR op -> Printf.sprintf "STR %s, %s, %d" (Registers.name op.sr) (Registers.name op.base_r) op.pc_offset 
  | OP_TRAP op -> Printf.sprintf "TRAP %#X" op.vect