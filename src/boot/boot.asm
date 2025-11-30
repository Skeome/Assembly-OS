; ==================================================================
; Yggdrasil ][ - Stage 1 Bootloader
; The "Surface Interface"
; ==================================================================
[ORG 0x7C00]        ; BIOS always loads us here
[BITS 16]           ; We start in 16-bit Real Mode

; --- Constants ---
LOADER_OFFSET equ 0x1000 ; We will load Stage 2 at memory address 0x1000
LOADER_SIZE equ 20       ; Read 20 sectors (10KB for loader and kernel)

start:
    ; 1. Save the Boot Drive Number
    ; The BIOS stores the drive number it booted from in DL.
    ; We must save this, because other instructions might overwrite DL.
    mov [BOOT_DRIVE], dl

    ; 2. Setup Segment Registers
    xor ax, ax      ; AX = 0
    mov ds, ax      ; Data Segment = 0
    mov es, ax      ; Extra Segment = 0
    mov ss, ax      ; Stack Segment = 0
    mov sp, 0x7C00  ; Stack grows down from where we loaded. Safe.

    ; 3. Reset Disk Controller
    mov ah, 0x00    ; INT 13h AH=0 (Reset)
    mov dl, [BOOT_DRIVE] ; Use the saved drive number
    int 0x13
    jc disk_error   ; If Carry Flag is set, something broke.

    ; 4. Load Stage 2 (loader.asm) from disk
    mov bx, LOADER_OFFSET ; ES:BX = Buffer Address (0x0000:0x1000)
    mov ah, 0x02          ; INT 13h AH=2 (Read Sectors)
    mov al, LOADER_SIZE   ; Number of sectors to read
    mov ch, 0x00          ; Cylinder 0
    mov dh, 0x00          ; Head 0
    mov cl, 0x02          ; Sector 2 (Sector 1 is this bootloader)
    mov dl, [BOOT_DRIVE]  ; Use the saved drive number
    int 0x13
    jc disk_error         ; Error check

    ; 5. The "Dive" - Jump to Stage 2
    mov si, msg_success
    call print_string
    
    jmp LOADER_OFFSET     ; Transfer control to 0x1000

; --- Subroutines ---

disk_error:
    mov si, msg_error
    call print_string
    jmp $               ; Infinite loop (Hang the system)

print_string:
    ; Basic BIOS teletype output
    mov ah, 0x0E
.loop:
    lodsb               ; Load byte at DS:SI into AL
    or al, al           ; Check if Zero (End of string)
    jz .done
    int 0x10            ; BIOS Interrupt: Print Character
    jmp .loop
.done:
    ret

; --- Data ---
BOOT_DRIVE  db 0 ; Variable to store the drive number
msg_success db 'Yggdrasil ][ Initialized...', 0x0D, 0x0A, 0
msg_error   db 'Disk Error. Halting.', 0

; --- Padding & Magic Number ---
times 510 - ($ - $$) db 0 ; Pad remaining bytes with 0
dw 0xAA55                 ; The Boot Signature (Required by BIOS)