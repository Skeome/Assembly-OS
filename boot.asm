; boot.asm - Stage 1 (LBA Read)
; Job: Load kernel.bin (Stage 2) into memory at 0x1000 using LBA.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

KERNEL_START_LBA        equ 0x01      ; LBA 1 (Sector 2 on disk)
KERNEL_SECTORS_TO_READ  equ 40        ; 40 sectors (20KB for kernel.asm)
KERNEL_LOAD_SEGMENT     equ 0x0100    ; ES = 0x0100 (Physical address 0x1000)
KERNEL_LOAD_OFFSET      equ 0x0000    ; BX = 0x0000

start:
    cli                     ; Disable interrupts
    
    ; Setup segments (DS=ES=SS=0) and stack
    xor ax, ax              
    mov ds, ax              
    mov es, ax              
    mov ss, ax              
    mov sp, 0x7B00          ; Use stack below bootloader (0x7C00)

    ; Save boot drive ID (DL) to a safe location
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
    
    ; --- The INT 0x13 / AH=0x42 Call (Load 40 sectors) ---
    mov ah, 0x42            ; Function 0x42: Extended Read Sectors (LBA)
    mov dl, [boot_drive_storage]

    mov dl, 0x80           ; Use first hard disk for testing
    
    int 0x13
    
    jnc .jump_to_stage2     ; If carry flag is clear, success!
    
    ; Read failed path
    mov al, 'E'
    call print_char
    mov si, msg_disk_error_load
    call print_string_halt
    
.jump_to_stage2:
    ; --- CHECKPOINT C: Load complete ---
    mov al, 'C'
    call print_char

    ; --- CRITICAL FIX: Set DS and ES to the new segment (0x0100) ---
    mov ax, KERNEL_LOAD_SEGMENT
    mov ds, ax              ; Set DS to 0x0100
    mov es, ax              ; Set ES to 0x0100

    ; --- 3. Jump to the loaded kernel (FIXED: Using push/retf) ---
    push KERNEL_LOAD_SEGMENT ; Pushes 0x0100 (CS) onto the stack
    push KERNEL_LOAD_OFFSET  ; Pushes 0x0000 (IP) onto the stack
    retf                     ; Performs a Far Return (effectively a Far Jump)

; --------------------------------------
; Utilities, Data, and Padding
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

; Disk Address Packet (DAP) Structure for AH=0x42 (16 bytes)
dap_packet:
dap_size:           db 0x10         
dap_reserved:       db 0x00         
dap_sectors:        dw 0x0000       
dap_buffer_offset:  dw 0x0000       
dap_buffer_segment: dw 0x0000       
dap_lba_low:        dd 0x00000000   
dap_lba_high:       dd 0x00000000   

; Padding and Signature
times 510 - ($ - $$) - 1 db 0 
boot_drive_storage: db 0        ; Store drive ID here (at 0x7DFD)
dw 0xAA55                       ; Boot signature