## lc3vm 
Implementation of LC-3 educational architecture virtual machine in OCaml. This implementation includes disassembler.

## Usage
``usage: lc3vm exec objfile | lc3vm disasm objfile``

## Test suite
The implementation includes a number of correctness tests written in LC-3 assembly language built with https://lc3.cs.umanitoba.ca/ (go see [binaries](https://github.com/nvevg/lc3vm/tree/master/binaries))

## Can it run Doom?
It can't. But it can run [rogue.obj](https://github.com/nvevg/lc3vm/tree/master/binaries/rogue.obj) and [2048.obj](https://github.com/nvevg/lc3vm/tree/master/binaries/2048.obj) - these only two "real" examples of software I was able to find.

## Limitations
- This implementation doesn't use any preloaded OS, so all TRAP routines are implemented in the VM (runtime) itself
- As a consequence of the previous point, RTI opcode aborts the execution since there is no privileges
- Display data/display status register are not supported

## Demo
[![asciicast](https://asciinema.org/a/900297.svg)](https://asciinema.org/a/900297)
