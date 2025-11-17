#!/bin/sh

# Stop on first error
set -e

# 1. Assemble the bootloader (boot.asm)
nasm -f bin -o boot.bin -l boot.lst boot.asm

cp boot.lst boot.lst.txt

# 2. Assemble the kernel loader (kernel.asm)
nasm -f bin -o kernel.bin kernel.asm

# 3. Assemble the 32-bit kernel (kernel32.asm)
nasm -f bin -o kernel32.bin kernel32.asm

# 4. Concatenate kernel.bin and kernel32.bin into one file
cat kernel.bin kernel32.bin > system.bin

# 5. Create an empty 1.44MB floppy disk image
dd if=/dev/zero of=boot.img bs=1024 count=1440

# 6. Write the bootloader to the first sector (LBA 0)
dd if=boot.bin of=boot.img bs=512 count=1 seek=0 conv=notrunc

# 7. Write the combined system.bin (kernel + kernel32) to the disk
#    starting at sector 1 (the sector after the bootloader)
dd if=system.bin of=boot.img bs=512 seek=1 conv=notrunc

echo "Success! 'boot.img' created."