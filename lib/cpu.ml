open Utils

type memory = int array

type t = { 
  pc : int; 
  registers : Registers.t; 
  cond: int;
  mem : memory; 
  halted: bool; 
}

let from_image (image_path : string) =
  let memory = Array.make Const.memory_max 0 in
  In_channel.with_open_bin image_path (fun ic ->
    let tmp = Bytes.create 2 in
    match In_channel.really_input ic tmp 0 2 with
    | Some () ->
      let origin = Bytes.get_uint16_be tmp 0 in
      let to_read = Const.memory_max - origin in
      let buf = Bytes.create to_read in
      let nread = In_channel.input ic buf 0 to_read in
      let nwords = nread / 2 in
      for i = 0 to nwords - 1 do
        let word = Bytes.get_uint16_be buf (i * 2) in
        memory.(origin + i) <- word
      done;
      Ok { pc = origin; registers = Registers.make (); mem = memory; cond = Const.cond_zero; halted = false; }
    | None -> Error `FailedToReadImage
  )
;;

let check_key (timeout: float)=
  match Unix.select [ Unix.stdin ] [] [] timeout with
  | [], _, _ -> false
  | _ :: _, _, _ -> true
;;

let read_stdin_char_nonblock () =
  let buf = Bytes.create 1 in
  match Unix.read Unix.stdin buf 0 1 with
  | 1 -> Some (Char.code (Bytes.get buf 0))
  | _ -> None
;;

let read_stdin_char () = 
  let rec poll () = 
    if check_key (-1.0) then 
        match read_stdin_char_nonblock () with
        | Some ch -> ch
        | None -> poll ()
    else poll ()
    in
    poll ()
;;

let write_stdout_char ch = 
  let buf = Bytes.make 1 ch in
  ignore (Unix.write Unix.stdout buf 0 1)
;;

let write_stdout_string s =
  let len = String.length s in
  let rec aux off =
    if off < len then
      let n = Unix.write_substring Unix.stdout s off (len - off) in
      aux (off + n)
  in
  aux 0
;;

let read_mem (cpu : t) (addr : int) =
  if addr = Const.kbsr_mem_loc then begin
    if check_key (0.05) then begin
      match read_stdin_char_nonblock () with
      | Some ch -> (cpu.mem.(Const.kbdr_mem_loc) <- ch; cpu.mem.(Const.kbsr_mem_loc) <- (1 lsl 15))
      | None -> ()
    end else
      cpu.mem.(Const.kbsr_mem_loc) <- 0x0000
  end;
  if addr >= 0 && addr < Const.memory_max then
    Ok cpu.mem.(addr)
  else
    Error (`InvalidMemoryAccess (addr, cpu.pc))
;;

let write_mem (cpu: t) (addr: int) (v: int) = 
  if addr < 0 || addr >= Const.memory_max then Error (`InvalidMemoryAccess (addr, cpu.pc)) else (cpu.mem.(addr) <- (v land 0xFFFF); Ok cpu)
;;

let update_flags (dr: Registers.register) (cpu: t) = 
  let reg_val = Registers.get ~register:dr cpu.registers in
  let flag = 
    if reg_val = 0x00 then Const.cond_zero
    else if Utils.bits ~pos:15 ~width: 1 reg_val <> 0 then Const.cond_negative
    else Const.cond_positive
  in
  { cpu with cond = flag }
;; 

let exec_lea (cpu: t) (op: Opcode.lea_op) = 
  let addr = cpu.pc +^ op.pc_offset in 
  { cpu with registers = Registers.set ~register:op.dr ~value:addr cpu.registers }
  |> update_flags op.dr
;;

let exec_trap_puts (cpu : t) =
  let rec loop addr =
    if addr < 0 || addr >= Const.memory_max then
      Error (`InvalidMemoryAccess (addr, cpu.pc))
    else
      let word = cpu.mem.(addr) land 0xFFFF in
      let ch = word land 0xFF in
      if ch = 0x00 then
        Ok ()
      else
        let () = output_char stdout (char_of_int ch) in
        loop ((addr + 1) land 0xFFFF)
  in
  let start = Registers.get ~register:Registers.R_R0 cpu.registers land 0xFFFF in
  let* () = loop start in
  flush stdout;
  Ok cpu
;;

let exec_trap_halt (cpu: t) = 
  write_stdout_string "\nHALT\n";
  Ok { cpu with halted = true }
;;

let exec_trap_getc (cpu: t) =
  let ch = read_stdin_char () in
  let new_regs = Registers.set ~register:Registers.R_R0 ~value:(ch land 0xFF) cpu.registers in
  Ok { cpu with registers = new_regs }
;;

let exec_trap_out (cpu: t) = 
  let r0 = Registers.get ~register:Registers.R_R0 cpu.registers in
  let val8 = r0 land 0xFF in
  let char_val = char_of_int val8 in 
  write_stdout_char char_val;
  Ok cpu;
;;

let exec_trap_in (cpu: t) = 
  write_stdout_string "Enter a character: ";
  let ch = read_stdin_char () in 
  write_stdout_char (char_of_int ch);
  let regs = Registers.set ~register:Registers.R_R0 ~value:(ch land 0xFF) cpu.registers in
  Ok { cpu with registers = regs }
;;

let exec_trap_putsp (cpu : t) =
  let rec loop addr =
    if addr < 0 || addr >= Const.memory_max then
      Error (`InvalidMemoryAccess (addr, cpu.pc))
    else
      let word = cpu.mem.(addr) land 0xFFFF in
      if word = 0x0000 then
        Ok ()
      else
        let lo = word land 0xFF in
        let hi = (word lsr 8) land 0xFF in
        write_stdout_char (char_of_int lo);
        if hi <> 0 then
          write_stdout_char(char_of_int hi);
        loop ((addr + 1) land 0xFFFF)
  in
  let start = Registers.get ~register:Registers.R_R0 cpu.registers land 0xFFFF in
  let* () = loop start in
  Ok cpu
;;

let exec_trap (cpu: t) (vect: int) = 
  let* trap = Trap.trap_from_int vect in
  match trap with 
  | Trap.TRAP_PUTS -> exec_trap_puts cpu
  | Trap.TRAP_GETC -> exec_trap_getc cpu
  | Trap.TRAP_OUT -> exec_trap_out cpu
  | Trap.TRAP_IN -> exec_trap_in cpu
  | Trap.TRAP_PUTSP -> exec_trap_putsp cpu
  | Trap.TRAP_HALT -> exec_trap_halt cpu
;;

let get_value (cpu: t) (v: Opcode.register_or_value) =
  match v with
  | Register reg -> Registers.get ~register:reg cpu.registers
  | Value ival -> ival
;;

let exec_add (cpu: t) (op: Opcode.binary_op) = 
  let rhs = get_value cpu op.sr2 in
  let lhs = Registers.get ~register:op.sr1 cpu.registers in
  { cpu with registers = Registers.set ~register:op.dr ~value:(lhs +^ rhs) cpu.registers }
  |> update_flags op.dr
;;

let exec_and (cpu: t) (op: Opcode.binary_op) = 
  let rhs = get_value cpu op.sr2 in
  let lhs = Registers.get ~register:op.sr1 cpu.registers in
  { cpu with registers = Registers.set ~register:op.dr ~value:(lhs land rhs) cpu.registers }
  |> update_flags op.dr
;;

let exec_not (cpu: t) (op: Opcode.unary_op) = 
  let srv = Registers.get ~register:op.sr cpu.registers in
  let compliment = lnot srv in
  { cpu with registers = Registers.set ~register:op.dr ~value:compliment cpu.registers } 
  |> update_flags op.dr
;;

let exec_br (cpu: t) (op: Opcode.br_op) = 
  let zero = (cpu.cond = Const.cond_zero && op.z) in
  let negative = (cpu.cond = Const.cond_negative && op.n) in
  let positive = (cpu.cond = Const.cond_positive && op.p) in
  if (zero || positive || negative) then { cpu with pc = cpu.pc +^ op.pc_offset } 
  else cpu
;;

let exec_jmp (cpu: t) (op: Opcode.jmp_op) =
  let reg = Registers.get ~register:op.base_r cpu.registers in
  { cpu with pc = reg }
;;

let exec_ret (cpu: t) = { cpu with pc = Registers.get ~register:Registers.R_R7 cpu.registers };;

let exec_jsr (cpu: t) (op: Opcode.jsr_op) = 
  let regs = Registers.set ~register:Registers.R_R7 ~value:cpu.pc cpu.registers in
  { cpu with registers = regs; pc = cpu.pc +^ op.pc_offset };;

let exec_jsrr (cpu: t) (op: Opcode.jsrr_op) = 
  let regs = Registers.set ~register:Registers.R_R7 ~value:cpu.pc cpu.registers in
  { cpu with registers = regs; pc = Registers.get ~register:op.base_r cpu.registers };;

let exec_ld (cpu: t) (op: Opcode.ld_op) = 
  let addr = cpu.pc +^ op.pc_offset in
  let* mem = read_mem cpu addr in
  Ok ({ cpu with registers = Registers.set ~register:op.dr ~value:mem cpu.registers } |> update_flags op.dr)
;;

let exec_ldi (cpu: t) (op: Opcode.ldi_op) = 
  let* addr1 = read_mem cpu (cpu.pc +^ op.pc_offset) in
  let* value = read_mem cpu addr1 in 
  Ok ({ cpu with registers = Registers.set ~register:op.dr ~value:value cpu.registers } |> update_flags op.dr)
;;

let exec_ldr (cpu: t) (op: Opcode.ldr_op) = 
  let addr = op.offset +^ Registers.get ~register:op.base_r cpu.registers in
  let* value = read_mem cpu addr in
  Ok ({ cpu with registers = Registers.set ~register:op.dr ~value:value cpu.registers } |> update_flags op.dr)
;;

let exec_rti (cpu: t) = Error (`PrivilegeViolation cpu.pc);;

let exec_st (cpu: t) (op: Opcode.st_op) = 
  let addr = cpu.pc +^ op.pc_offset in
  let value = Registers.get ~register:op.sr cpu.registers in
  write_mem cpu addr value;
;;

let exec_sti (cpu: t) (op: Opcode.sti_op) = 
    let addr = cpu.pc +^ op.pc_offset in
    let* addr2 = read_mem cpu addr in
    let value = Registers.get ~register:op.sr cpu.registers in
    write_mem cpu addr2 value;
;;

let exec_str (cpu: t) (op: Opcode.str_op) = 
  let addr = Registers.get ~register:op.base_r cpu.registers +^ op.pc_offset in
  let value = Registers.get ~register:op.sr cpu.registers in
  write_mem cpu addr value;
;;

let exec_op (cpu: t) (opcode: Opcode.opcode) = 
  match opcode with
  | Opcode.OP_ADD op -> Ok (exec_add cpu op)
  | Opcode.OP_AND op -> Ok (exec_and cpu op)
  | Opcode.OP_NOT op -> Ok (exec_not cpu op)
  | Opcode.OP_BR op -> Ok (exec_br cpu op)
  | Opcode.OP_JMP op -> Ok (exec_jmp cpu op)
  | Opcode.OP_JSR op -> Ok (exec_jsr cpu op)
  | Opcode.OP_JSRR op -> Ok (exec_jsrr cpu op)
  | Opcode.OP_RET _ -> Ok (exec_ret cpu)
  | Opcode.OP_LD op -> exec_ld cpu op
  | Opcode.OP_LDI op -> exec_ldi cpu op
  | Opcode.OP_LDR op -> exec_ldr cpu op
  | Opcode.OP_LEA op -> Ok (exec_lea cpu op)
  | Opcode.OP_TRAP op -> exec_trap cpu op.vect
  | Opcode.OP_RTI _ -> exec_rti cpu
  | Opcode.OP_ST op -> exec_st cpu op
  | Opcode.OP_STI op -> exec_sti cpu op
  | Opcode.OP_STR op -> exec_str cpu op
;;