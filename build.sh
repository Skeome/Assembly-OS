#!/bin/sh

# Stop on first error
set -e

# 1. Assemble the bootloader (boot.asm)
nasm -f bin -o boot.bin boot.asm

# 2. Assemble the kernel loader (kernel.asm)
nasm -f bin -o kernel.bin kernel.asm

# 3. Assemble the 32-bit kernel (kernel32.asm)
nasm -f bin -o kernel32.bin kernel32.asm

# 4. Create an empty 1.44MB floppy disk image
dd if=/dev/zero of=boot.img bs=1024 count=1440

# 5. Write the bootloader to the first sector (LBA 0)
dd if=boot.bin of=boot.img bs=512 count=1 seek=0 conv=notrunc

# 6. Write the kernel loader to the second sector (LBA 1)
#    (It's 40 sectors long, so it will occupy LBA 1 to 40)
dd if=kernel.bin of=boot.img bs=512 seek=1 conv=notrunc

# 7. Write the 32-bit kernel starting at sector 42 (LBA 41)
#    This is immediately after the 40 sectors of kernel.bin
dd if=kernel32.bin of=boot.img bs=512 seek=41 conv=notrunc

echo "Success! 'boot.img' created."