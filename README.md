# Yggdrasil OS - Bare-Metal Foundation

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

