#!/usr/bin/env zsh

# ==================================================================
# AtlantOS Build Script (Kernel Support)
# ==================================================================

# --- Configuration ---
ASM="nasm"

# QEMU Debug Flags
EMU="qemu-system-i386"
EMU_FLAGS=(
    "-d" "int,cpu_reset,guest_errors"
    "-no-reboot"
    "-D" "qemu.log"
)

SRC_BOOT="src/boot"
SRC_KERNEL="src/kernel"
BUILD_DIR="build"
OS_IMAGE="$BUILD_DIR/atlantos-core.img"

# --- Functions ---

clean() {
    echo "[*] Cleaning up..."
    rm -rf "$BUILD_DIR"
    rm -f qemu.log
}

build() {
    mkdir -p "$BUILD_DIR"

    # 1. Assemble Bootloader (Stage 1)
    echo "[*] Assembling Bootloader..."
    $ASM -f bin "$SRC_BOOT/boot.asm" -o "$BUILD_DIR/boot.bin" \
         -l "$BUILD_DIR/boot.lst"
    
    if [[ $? -ne 0 ]]; then echo "Error assembling boot.asm"; exit 1; fi

    # 2. Assemble Loader (Stage 2)
    echo "[*] Assembling Loader..."
    $ASM -f bin "$SRC_BOOT/loader.asm" -o "$BUILD_DIR/loader.bin" \
         -l "$BUILD_DIR/loader.lst"
    
    if [[ $? -ne 0 ]]; then echo "Error assembling loader.asm"; exit 1; fi

    # 3. Assemble Kernel (Stage 3)
    echo "[*] Assembling Kernel..."
    # We manually ensure the directory exists just in case
    mkdir -p "$SRC_KERNEL"
    $ASM -f bin "$SRC_KERNEL/core.asm" -o "$BUILD_DIR/kernel.bin" \
         -l "$BUILD_DIR/kernel.lst"

    if [[ $? -ne 0 ]]; then echo "Error assembling core.asm"; exit 1; fi

    # 4. Create Disk Image
    echo "[*] Linking Stages..."
    
    # CRITICAL: Pad Loader to 4096 bytes (0x1000)
    # Loader starts at 0x1000. If we pad it to 0x1000 length,
    # The Kernel will naturally start at 0x2000.
    truncate -s 4096 "$BUILD_DIR/loader.bin"

    # Concatenate: Boot -> Loader (padded) -> Kernel
    cat "$BUILD_DIR/boot.bin" "$BUILD_DIR/loader.bin" "$BUILD_DIR/kernel.bin" > "$OS_IMAGE"
    
    # Final Pad to 1.44MB (Floppy size)
    truncate -s 1474560 "$OS_IMAGE"
    
    echo "[+] Build Complete: $OS_IMAGE"
    echo "[+] Debug Lists: $BUILD_DIR/"
}

run_vm() {
    echo "[*] Booting AtlantOS..."
    $EMU -drive format=raw,file="$OS_IMAGE",index=0,if=floppy $EMU_FLAGS
}

# --- Execution Flow ---

case "$1" in
    "clean")
        clean
        ;;
    "run")
        build
        run_vm
        ;;
    *)
        build
        echo "Usage: ./build.sh [run|clean]"
        ;;
esac