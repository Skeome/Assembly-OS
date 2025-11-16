# 1. The Bootloader: boot.asm
This file's code is exactly 512 bytes. The BIOS (the chip on the motherboard) loads only this file from the floppy disk, puts it at the physical memory address 0x7C00, and tells the CPU to start running it. Its only job is to load the next, bigger file (our kernel).

Code snippet:
```
; ------------------------------------------------------------------
; Stage 1 Boot Sector (Optimized Loader)
;
; Assumes success and loads Stage 2 ("kernel.bin") from the
; floppy disk into memory at 0x5000, then executes it.
; ------------------------------------------------------------------

org 0x7C00
```
- `org 0x7C00`: This is the "Origin" directive. It tells the assembler, "Don't assume this code runs at address 0. Assume it runs at `0x7C00`." This is critical so that when we reference data, the correct addresses are calculated. It's 0x7C00 because that's the **non-negotiable** standard address where the BIOS loads boot sectors.

Code snippet:
```
bits 16
```
- `bits 16`: This tells the assembler to generate 16-bit code. When a PC boots, it's in "16-bit Real Mode," a backwards-compatible mode that mimics the 8086 processor from 1978.

Code snippet:
```
start:
    ; --- Setup Stack & Segments ---
    ; Set all segments to 0 and create a stack
    ; that starts at 0x7C00 and grows downwards.
    cli           ; Disable interrupts
```
- `cli`: "Clear Interrupt Flag." This temporarily disables all hardware interrupts (like keyboard, timer, etc.). We do this because we're about to change the segment registers, which is a very sensitive operation. If an interrupt happened while our segments were in a weird state, the computer would crash.

Code snippet:
```
    mov ax, 0x0000
    mov ds, ax    ; Data Segment
    mov es, ax    ; Extra Segment
    mov ss, ax    ; Stack Segment
```
- `mov ds, ax` etc.: We set all our "segment" registers (Data, Extra, Stack) to 0. In 16-bit mode, memory is accessed via `Segment:Offset`. This simplifies things, making our segments start at physical address 0. (e.g., DS:0x1000 now points to physical address 0x1000).

Code snippet:
```
    mov sp, 0x7C00 ; Stack Pointer. Stack is now at 0000:7C00.
```
- `mov sp, 0x7C00`: This sets up our "**Stack Pointer**." The stack is a temporary area for storing data. We point it to the start of our own code. Since stacks on x86 grow *downwards*, our stack will use the memory below 0x7C00, which is safe for now.

Code snippet:
```
    sti           ; Re-enable interrupts
```
- `sti`: "Set Interrupt Flag." Our critical setup is done, so we re-enable interrupts.

Code snippet:
```
    ; --- Load Stage 2 (Kernel) ---
    ; (ah=0x02, al=sectors, ch=track, cl=sector, dh=head, dl=drive)
    ; (es:bx = destination buffer)
    mov ax, 0x0500
    mov es, ax      ; Destination Segment = 0x0500
    mov bx, 0x0000  ; Destination Offset = 0x0000 (loads at 0x5000)
```
- `mov es, ax` / `mov bx, 0x0000`: This sets our destination buffer. The BIOS read command (`int 0x13`) needs to know where to put the data it reads. It uses the `ES:BX` register pair.

- We set `ES` to `0x0500` and `BX` to `0x0000`. The physical address is calculated as `(ES * 16) + BX`, which is `(0x0500 * 16) + 0x0000 = 0x5000`.

- **Why `0x5000`?** It's an arbitrary but safe address. It's far away from the BIOS data area (at `0x0000`), our bootloader (at `0x7C00`), and video memory (at `0xA0000` or `0xB8000`). It's just a safe, open chunk of RAM to load our kernel into.

Code snippet:
```
    mov ah, 0x02    ; Function: "Read Sectors"
    mov al, 10      ; Sectors to read (40 * 512b = 20KB)
    mov ch, 0       ; Track 0
    mov cl, 2       ; Sector 2 (Sector 1 is us)
    mov dh, 0       ; Head 0
    mov dl, 0x00    ; Drive 0 (Floppy A:)
```
These are all parameters for the BIOS disk interrupt.
- `ah = 0x02`: "Read Sectors" function.
- `al = 40`: Read 40 sectors. (40 sectors * 512 bytes/sector = 20480 bytes, or 5KB).
- `ch = 0`, `cl = 2`, `dh = 0`: This is the location. Track 0, Head 0.

- - **Why Sector 2 (`cl = 2`)?** Because Sector 1 is the bootloader itself! We're reading the file that comes after that on the disk.

- `dl = 0x00`: Drive 0, which is `A:`.

Code snippet:
```
    int 0x13        ; Call BIOS disk interrupt
```
- `int 0x13`: "Interrupt 13h." This is the "magic number" that tells the CPU to pause, find the BIOS Disk Service routine, and run it. This one call does all the heavy lifting of spinning up the disk and copying the data into `ES:BX` (`0x5000`).

Code snippet:
```
    ; --- Assume Success and Jump to Stage 2 ---
    ; We've removed all error checking per the plan.
    jmp 0x0500:0x0000 ; Jump to the start of our kernel.
```
- `jmp 0x0500:0x0000`: This is the "far jump." It's the most important line. It does two things:

- - Sets the Code Segment (`CS`) register to `0x0500`.

- - Sets the Instruction Pointer (`IP`) register to `0x0000`.

- The CPU will now fetch its next instruction from `CS:IP`, which is physical address `(0x0500 * 16) + 0x0000 = 0x5000`. This is the very first byte of the kernel we just loaded. We have successfully passed control.

Code snippet:
```
; --- Boot Sector Padding and Magic Number ---
times 510 - ($ - $$) db 0
```
- `times 510 - ($ - $$) db 0`: This is a command for the assembler.

- `$` means "current address."

- `$$` means "start address of this section" (which is 0x7C00).

- `($ - $$)` calculates how many bytes of code we've written so far.

`510 - ...` calculates how many more bytes we need to add to reach byte 510.

`times ... db 0`: Fills all those empty bytes with `0`. This pads our file.

Code snippet:
```
dw 0xAA55
```
- `dw 0xAA55`: "Define Word." This writes the 2-byte "magic number" `0xAA55` at the very end of our 512-byte sector (at bytes 510 and 511). The BIOS requires this. If it loads a sector and doesn't see `0xAA55` at the end, it considers the disk "not bootable" and gives up.

# 2. The Kernel: kernel.asm


Yggdrasil OS - Bare-Metal Foundation

This is the refactored, bare-metal foundation for the Inner Garden Foundation's OS.

New Architecture (3-Stage Load)

This design is robust, minimal, and secure. Each stage has one job.

Stage 1: boot.asm (512 bytes)

Location: Master Boot Record (Sector 1).

Mode: 16-bit Real Mode.

Job: Loads kernel.bin (Stage 2) into memory at physical address 0x0500 and jumps to it. It also stores the boot drive ID at 0x7BFE for Stage 2 to use.

Stage 2: kernel.asm (Padded to 20KB)

Location: Sectors 2-41 on the disk.

Mode: Starts in 16-bit Real Mode, transitions to 32-bit Protected Mode.

Job:

Retrieves the boot drive ID from 0x7BFE.

Loads the 32-bit Kernel (kernel32.bin) from disk (Sectors 42+) into memory at 0x10000.

Enables the A20 line (to access memory > 1MB).

Loads the Global Descriptor Table (GDT).

Crucially, it now sets SS (Stack Segment) to a valid data descriptor before enabling protected mode.

Switches the CPU to 32-bit Protected Mode.

Performs a far jump (retf) to the 32-bit code segment, which is now safe because SS is valid.

Jumps to the main kernel at 0x10000.

Stage 3: kernel32.asm (32-bit Yggdrasil Core)

Location: Starts at Sector 42.

Mode: 32-bit Protected Mode.

Job: This is the OS. It starts executing at 0x10000, safe in high memory, with full 32-bit addressing. It sets up its own stack, clears the screen, and prints a welcome message.

Build Process

You will need nasm (The Netwide Assembler) and dd (a standard Unix utility).

#!/bin/bash

# --- 1. Assemble Boot Sector (Stage 1) ---
nasm -f bin boot.asm -o boot.bin

# --- 2. Assemble Kernel Loader (Stage 2) ---
nasm -f bin kernel.asm -o kernel.bin

# --- 3. Assemble Yggdrasil Core (Stage 3) ---
nasm -f bin kernel32.asm -o kernel32.bin

# --- 4. Create a blank 1.44MB floppy disk image ---
# 1440 blocks * 1024 bytes/block = 1474560 bytes
dd if=/dev/zero of=boot.img bs=1024 count=1440

# --- 5. Write the bootloader to the MBR (Sector 1) ---
# conv=notrunc means 'do not truncate the output file'
dd if=boot.bin of=boot.img conv=notrunc

# --- 6. Write the kernel loader (Stage 2) ---
# seek=1 means write at the *second* 512-byte sector
dd if=kernel.bin of=boot.img bs=512 seek=1 conv=notrunc

# --- 7. Write the Yggdrasil Core (Stage 3) ---
# seek=41 means write at the *42nd* 512-byte sector
# (Sector 1 is boot.bin, Sectors 2-41 is kernel.bin (40 sectors), so kernel32 starts on 42)
dd if=kernel32.bin of=boot.img bs=512 seek=42 conv=notrunc

echo "Yggdrasil OS image 'boot.img' created successfully."

