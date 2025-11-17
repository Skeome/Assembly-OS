; boot.asm - Stage 1 (Minimal LBA Read)
; Job: Load kernel.bin (Stage 2) into memory at 0x1000 using LBA.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

KERNEL_START_LBA        equ 0x01      ; LBA 1 (Sector 2 on disk)
KERNEL_SECTORS_TO_READ  equ 40        ; 40 sectors (20KB for kernel.asm)
KERNEL_LOAD_SEGMENT     equ 0x0100    ; Physical address 0x1000
KERNEL_LOAD_OFFSET      equ 0x0000    

start:
    cli                     ; Disable interrupts
    
    ; 1. Setup Segments and Stack
    xor ax, ax              
    mov ds, ax              ; DS=0
    mov es, ax              ; ES=0
    mov ss, ax              ; SS=0
    mov sp, 0x7C00          ; Stack starts just below 0x7C00 (grows down)

    ; Save boot drive ID (DL) to 0x7DFD
    mov [boot_drive_storage], dl
    
    sti                     ; Re-enable interrupts

    ; 2. LBA Read Setup
    
    ; Setup Buffer Address (ES:BX = 0x0100:0x0000 -> 0x1000 physical)
    mov ax, KERNEL_LOAD_SEGMENT
    mov es, ax
    mov bx, KERNEL_LOAD_OFFSET
    
    ; Setup Disk Address Packet (DAP) at DS:SI (0x0000:0x7C98 in low memory)
    mov si, dap_packet
    
    ; Populate DAP with target values
    mov word [si + 0x02], KERNEL_SECTORS_TO_READ ; dap_sectors (40)
    mov word [si + 0x04], bx                     ; dap_buffer_offset (0)
    mov word [si + 0x06], es                     ; dap_buffer_segment (0x0100)
    mov dword [si + 0x08], KERNEL_START_LBA      ; dap_lba_low (1)
    
    ; 3. Execute LBA Read (INT 0x13 / AH=0x42)
    mov ah, 0x42            ; Extended Read Sectors
    mov dl, [boot_drive_storage] ; Load original drive ID (usually 0x00)
    
    mov dl, 0x80            ; CRITICAL WORKAROUND: Use Hard Disk ID for LBA read in QEMU
    
    int 0x13
    
    jnc .jump_to_stage2     ; If carry flag is clear, success!
    
    ; Read failed: Halt indefinitely
    cli
    hlt
    jmp $
    
.jump_to_stage2:
    
    ; 4. Final Segment Setup (CRITICAL FIXES)
    ; DS and ES must match the new code segment (0x0100) before the jump.
    mov ax, KERNEL_LOAD_SEGMENT
    mov ds, ax              ; Set DS to 0x0100
    mov es, ax              ; Set ES to 0x0100

    ; 5. Jump to the loaded kernel (Stage 2)
    push KERNEL_LOAD_SEGMENT ; Pushes 0x0100 (CS) onto the stack
    push KERNEL_LOAD_OFFSET  ; Pushes 0x0000 (IP) onto the stack
    retf                     ; Far Return/Jump to 0x0100:0x0000

; --------------------------------------
; Data and Padding
; --------------------------------------

; Disk Address Packet (DAP) Structure for AH=0x42 (16 bytes)
dap_packet:
dap_size:           db 0x10         ; 0x00 (Size)
dap_reserved:       db 0x00         ; 0x01
dap_sectors:        dw 0x0000       ; 0x02 (Filled by code)
dap_buffer_offset:  dw 0x0000       ; 0x04 (Filled by code)
dap_buffer_segment: dw 0x0000       ; 0x06 (Filled by code)
dap_lba_low:        dd 0x00000000   ; 0x08 (Filled by code)
dap_lba_high:       dd 0x00000000   ; 0x0C

; Padding and Signature
times 510 - ($ - $$) - 1 db 0 
boot_drive_storage: db 0        ; Store drive ID here (at 0x7DFD)
dw 0xAA55                       ; Boot signature