# `Yggdrasil ][` - Codename AtlantOS - Bare-Metal Foundation
This is the bare-metal foundation for the Inner Garden Foundation's OS.
Architecture (3-Stage Load)
This design is robust, minimal, and secure. Each stage has one job.

## Stage 1:
boot.asm (512 bytes)

### Location:
Master Boot Record (Sector 1).

### Mode:
16-bit Real Mode.

### Job:
Loads loader.asm (Stage 2) into memory at physical address 0x1000 and jumps to it.

## Stage 2:
loader.asm

### Location:
Sectors 2-5 on the disk.

### Mode:
Starts in 16-bit Real Mode, transitions to 32-bit Protected Mode.

### Job:
- Loads the 32-bit Kernel (core.asm) from disk (Sectors 6-20) into memory at 0x10000.
- Enables the A20 line (to access memory > 1MB).
- Loads the Global Descriptor Table (GDT).
- Sets EBP and ESP (Stack) to an address at the top of memory (0x90000) before enabling protected mode.
- Switches the CPU to 32-bit Protected Mode.
- Jumps to the main kernel at 0x2000.

## Stage 3:
core.asm 
32-bit `Yggdrasil ][` Core

### Location:
Sectors 6-20.

### Mode:
32-bit Protected Mode.

### Job:
This is the OS. It starts executing at 0x2000 with full 32-bit addressing. It currently clears the screen, and prints a status message.