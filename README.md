6502 CPU Emulator
=================
  
This package implements a 6502 CPU emulator as used in the Nintendo 
Entertainment System (NES).
  
It does not implement the illegal opcodes, and is not cycle accurate 
(but close enough).

The file `cpu_test.go` checks the output of the CPU instructions for 
`nestest.nes` rom against a pregenerated correct output. 
