; boot.asm - Stage 1 (Minimal LBA Read - Drive ID Fixed)
; Job: Load kernel.bin (Stage 2) into memory at 0x1000 using LBA.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

KERNEL_START_LBA        equ 0x01      ; LBA 1
KERNEL_SECTORS_TO_READ  equ 40        
KERNEL_LOAD_SEGMENT     equ 0x0100    ; ES = 0x0100 (Physical address 0x1000)
KERNEL_LOAD_OFFSET      equ 0x0000    

; Disk Address Packet (DAP) definition placed early to stabilize offsets
dap_packet:
dap_size:           db 0x10         ; 0x00: Packet size
dap_reserved:       db 0x00         ; 0x01
dap_sectors:        dw KERNEL_SECTORS_TO_READ ; 0x02: Sectors to read (40)
dap_buffer_offset:  dw KERNEL_LOAD_OFFSET     ; 0x04: Load offset (0x0000)
dap_buffer_segment: dw KERNEL_LOAD_SEGMENT    ; 0x06: Load segment (0x0100)
dap_lba_low:        dd KERNEL_START_LBA       ; 0x08: LBA Start (1)
dap_lba_high:       dd 0x00000000             ; 0x0C

; Padding and Drive ID Storage
times 510 - ($ - $$) - 1 db 0 
boot_drive_storage: db 0        ; Store drive ID here (at 0x7DFD)
dw 0xAA55                       ; Boot signature


start:
    cli                     ; Disable interrupts
    
    ; 1. Setup Segments and Stack
    xor ax, ax              
    mov ds, ax              ; DS=0 for accessing DAP
    mov es, ax              
    mov ss, ax              
    mov sp, 0x7C00          ; Stack starts just below 0x7C00

    ; Save boot drive ID (DL)
    mov [boot_drive_storage], dl
    
    sti                     ; Re-enable interrupts

    ; 2. Execute LBA Read (INT 0x13 / AH=0x42)
    
    ; Setup Buffer Address (ES:BX = 0x0100:0x0000 -> 0x1000 physical)
    mov ax, KERNEL_LOAD_SEGMENT
    mov es, ax
    mov bx, KERNEL_LOAD_OFFSET
    
    ; Setup DS:SI to point to the DAP structure (0x0000:dap_packet)
    mov si, dap_packet
    
    mov ah, 0x42            ; Extended Read Sectors
    
    ; --- The Fix: Temporarily use the Hard Disk ID for the AH=0x42 call ---
    ; --- We trust that the floppy BIOS should handle the ID conversion internally. ---
    mov dl, 0x80
    
    int 0x13
    
    ; Check drive ID again (in case BIOS reset it)
    mov dl, [boot_drive_storage] 
    
    jnc .jump_to_stage2     ; If carry flag is clear, success!
    
    ; Read failed: Halt indefinitely
    cli
    hlt
    jmp $
    
.jump_to_stage2:
    
    ; 3. Final Segment Setup (CRITICAL FIXES)
    ; DS and ES must match the new code segment (0x0100) before the jump.
    mov ax, KERNEL_LOAD_SEGMENT
    mov ds, ax              ; Set DS to 0x0100
    mov es, ax              ; Set ES to 0x0100

    ; 4. Jump to the loaded kernel (Stage 2)
    push KERNEL_LOAD_SEGMENT ; Pushes 0x0100 (CS) onto the stack
    push KERNEL_LOAD_OFFSET  ; Pushes 0x0000 (IP) onto the stack
    retf                     ; Far Return/Jump to 0x0100:0x0000