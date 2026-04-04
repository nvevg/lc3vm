open Lc3vm.Utils

let original_tio = Unix.tcgetattr Unix.stdin

let disable_input_buffering () =
  let open Unix in
  let new_tio = { original_tio with c_icanon = false; c_echo = false; } in
  tcsetattr stdin TCSANOW new_tio
;;

let restore_input_buffering () =
  let open Unix in
  tcsetattr stdin TCSANOW original_tio
;;

let handle_interrupt _ =
  restore_input_buffering ();
  print_newline ();
  exit (-2)
;;

let setup () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_interrupt);
  disable_input_buffering ()
;;

let disasm (state: Lc3vm.Cpu.t) =
  let handle_opcode pc res = 
    match res with
    | Ok opcode -> Printf.printf "%#X\t %s\n" pc (Lc3vm.Opcode.opcode_to_string opcode)
    | Error `UnusedOpcode -> Printf.printf "%#X\t Unused opcode, skipping\n" pc
    | Error `InvalidOpcode opw -> Printf.printf "%#X\t INVALID OPCODE (%s)\n" pc (Lc3vm.Utils.int_to_bstr opw)
    | Error `UnknownOpcode opcode -> Printf.printf "%#X\t UNKNOWN OPCODE %s\n" pc (Lc3vm.Utils.int_to_bstr opcode)
    | Error `UnknownRegister r -> Printf.printf "%#X\t INVALID REGISTER %d\n" pc r
  in
  let rec aux mem pc =
    if pc = Lc3vm.Const.memory_max then () 
    else
      let opw = mem.(pc) in
      handle_opcode pc (Lc3vm.Opcode.parse_opcode opw);
      aux mem (pc + 1)
    in aux state.mem state.pc
;; 

let execute (cpu: Lc3vm.Cpu.t) = 
  let unwrap s = 
    match s with
    | Ok prog -> prog
    | Error `IllegalOpcode op -> failwith (Printf.sprintf "lc3vm terminated abruptly: illegal opcode %s" (Lc3vm.Opcode.opcode_to_string op))
    | Error `InvalidMemoryAccess (addr, pc) -> failwith (Printf.sprintf "lc3vm terminated abruptly: invalid memory access (sigsegv); addr = %#X pc = %#X" addr pc)
    | Error `PrivilegeViolation pc -> failwith (Printf.sprintf "lc3vm terminated abruptly: RTI in user space mode; pc = %#X" pc)
    | Error _ -> failwith "lc3vm: unknown error"
  in
  let process_opcode (state: Lc3vm.Cpu.t) opcode  = 
    Lc3vm.Cpu.exec_op { state with pc = state.pc + 1 } opcode
  in
  let rec aux (state: Lc3vm.Cpu.t) = 
    if state.halted then Ok ()
    else if state.pc >= Lc3vm.Const.memory_max then failwith ("lc3vm: all code and no HALT")
    else
    let opw = state.mem.(state.pc) in
    Lc3vm.Opcode.parse_opcode opw
    |> Result.map (process_opcode state)
    |> Result.join
    |> unwrap
    |> aux
  in
  aux cpu
;;

let process t image_path =
  let* state = Lc3vm.Cpu.from_image image_path in
  match t with
  | `Disasm -> Ok (disasm state)
  | `Exec -> setup (); execute state
;;

let run_or_exit res =
  match res with
  | Ok () -> ()
  | Error _ -> exit 1
;;

let () =
  match Array.to_list Sys.argv with
  | _ :: "disasm" :: file :: _ -> run_or_exit (process `Disasm file)
  | _ :: "exec" :: file :: _ -> run_or_exit (process `Exec file)
  | _ ->
      prerr_endline "usage: lc3vm exec objfile | lc3vm disasm objfile";
      exit 1
;;