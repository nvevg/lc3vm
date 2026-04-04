## lc3vm 
Implementation of LC-3 educational architecture virtual machine in OCaml. This implementation includes disassembler.

## Usage
``usage: lc3vm exec objfile | lc3vm disasm objfile``

## Test suite
The implementation includes a number of correctness tests written in LC-3 assembly language built with https://lc3.cs.umanitoba.ca/ (go see [binaries](https://github.com/nvevg/lc3vm/tree/master/binaries))

## Limitations
- This implementation doesn't use any preloaded OS, so all TRAP routines are implemented in the VM (runtime) itself
- As a consequence of the previous point, RTI opcode aborts the execution
- Display data/display status register are not supported
