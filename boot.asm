; boot.asm
; Stage 1 - Master Boot Record (MBR)
; Loaded by BIOS at 0x7C00.
; Job: Load kernel.asm (Stage 2) using LBA addressing.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

KERNEL_START_LBA        equ 0x01      ; LBA 1 (Sector 2 on disk)
KERNEL_SECTORS_TO_READ  equ 40        ; 40 sectors (20KB for kernel.asm)
KERNEL_LOAD_SEGMENT     equ 0x0100    ; ES = 0x0100 (Physical address 0x1000)
KERNEL_LOAD_OFFSET      equ 0x0000    ; BX = 0x0000

start:
    cli                     ; Disable interrupts
    
    ; Setup segments for memory access (DS=0) and stack
    xor ax, ax              
    mov ds, ax              
    mov es, ax              
    mov ss, ax              
    mov sp, 0x7C00          ; Use high boot sector address for stack

    ; Save boot drive ID (DL) to a safe location (0x7DFD)
    mov [boot_drive_storage], dl
    
    sti                     ; Re-enable interrupts

    ; --- 1. Reset the disk controller ---
    mov ah, 0x00            ; Function 0x00: Reset Disk System
    mov dl, [boot_drive_storage]
    int 0x13
    
    ; --- CHECKPOINT A: Disk Reset OK ---
    mov al, 'A'
    call print_char

    ; --- 2. Load the kernel using LBA Read (AH=0x42) ---
    
    ; Setup Disk Address Packet (DAP)
    mov si, dap_packet      ; DS:SI points to DAP (DS=0 ensures correct physical address)
    
    ; Set Buffer Address (0x0100:0x0000 -> 0x1000)
    mov ax, KERNEL_LOAD_SEGMENT
    mov [dap_buffer_segment], ax
    mov word [dap_buffer_offset], KERNEL_LOAD_OFFSET
    
    ; Set LBA start and count
    mov word [dap_sectors], KERNEL_SECTORS_TO_READ 
    mov dword [dap_lba_low], KERNEL_START_LBA    
    
    ; --- The INT 0x13 / AH=0x42 Call (No Retries here, clean LBA read is reliable) ---
    mov ah, 0x42            ; Function 0x42: Extended Read Sectors (LBA)
    mov dl, [boot_drive_storage] 
    
    int 0x13
    
    jnc .jump_to_stage2     ; If carry flag is clear, success!
    
    ; Read failed path
    mov al, 'E'
    call print_char
    mov si, msg_disk_error_load
    call print_string_halt
    
.jump_to_stage2:
    ; --- CHECKPOINT B: Kernel Load OK ---
    mov al, 'B'
    call print_char
    
    ; --- CHECKPOINT C: Final Jump ---
    mov al, 'C'
    call print_char

    ; --- 3. Jump to the loaded kernel ---
    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

; --------------------------------------
; Utilities and Data
; --------------------------------------

print_char:                 
    mov ah, 0x0E
    int 0x10
    ret

print_string_halt:
    pusha
.loop:
    lodsb                   
    cmp al, 0
    je .halt
    call print_char
    jmp .loop
.halt:
    popa
    cli
    hlt
    jmp $

; --- Data ---
msg_disk_error_load:  db '1-Kernel read error! Halting.', 0

; Disk Address Packet (DAP) Structure for AH=0x42
dap_packet:
dap_size:           db 0x10         ; 0x10 (16 bytes)
dap_reserved:       db 0x00         
dap_sectors:        dw 0x0000       ; LBA Count
dap_buffer_offset:  dw 0x0000       
dap_buffer_segment: dw 0x0000       
dap_lba_low:        dd 0x00000000   ; LBA Start
dap_lba_high:       dd 0x00000000   

; Padding and Signature
times 510 - ($ - $$) - 1 db 0 
boot_drive_storage: db 0        ; Store drive ID here (at 0x7DFD)
dw 0xAA55                       ; Boot signature