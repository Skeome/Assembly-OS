# ==================================================================
# AtlantOS Build System
# ==================================================================

# --- Tools ---
ASM = nasm
EMU = qemu-system-i386

# --- Directories ---
SRC_BOOT = src/boot
BUILD_DIR = build

# --- Targets ---
# The final disk image
OS_IMAGE = $(BUILD_DIR)/atlantos-core.img

# Sources
BOOT_SRC = $(SRC_BOOT)/boot.asm
LOADER_SRC = $(SRC_BOOT)/loader.asm

# Binaries
BOOT_BIN = $(BUILD_DIR)/boot.bin
LOADER_BIN = $(BUILD_DIR)/loader.bin

# ==================================================================
# Rules
# ==================================================================

# Default target: Build the OS image
all: $(OS_IMAGE)

# 1. Run the Emulator
# Fixed: Added format=raw to silence QEMU warnings and ensure proper write access
# We explicitly mount it as a floppy (if=floppy) so BIOS assigns DL=0x00
run: $(OS_IMAGE)
	@echo "[*] Booting AtlantOS..."
	$(EMU) -d int -drive format=raw,file=$(OS_IMAGE),index=0,if=floppy

# 2. Create the Disk Image
# We combine boot.bin and loader.bin, then pad the rest with zeros
$(OS_IMAGE): $(BOOT_BIN) $(LOADER_BIN)
	@echo "[*] Creating Disk Image..."
	cat $(BOOT_BIN) $(LOADER_BIN) > $(OS_IMAGE)
# Pad to 1.44MB (Floppy size) to avoid disk read errors
	truncate -s 1474560 $(OS_IMAGE)

# 3. Assemble Bootloader (Stage 1)
$(BOOT_BIN): $(BOOT_SRC)
	@mkdir -p $(BUILD_DIR)
	@echo "[*] Assembling Bootloader..."
	$(ASM) -f bin $(BOOT_SRC) -o $(BOOT_BIN)

# 4. Assemble Loader (Stage 2)
$(LOADER_BIN): $(LOADER_SRC)
	@echo "[*] Assembling Loader..."
	$(ASM) -f bin $(LOADER_SRC) -o $(LOADER_BIN)

# Clean up build files
clean:
	@echo "[*] Cleaning up..."
	rm -rf $(BUILD_DIR)/*

.PHONY: all run clean