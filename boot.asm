; boot.asm - Stage 1 (Minimal LBA Read)
; Job: Load kernel.bin (Stage 2) into memory at 0x1000 using LBA.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

BOOT_SEGMENT_BASE       equ 0x07C0    ; 0x7C0 * 10h = 0x7C00 physical address

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

; Padding and Drive ID Storage (at 0x7DFD in memory)
times 510 - ($ - $$) - 1 db 0 
boot_drive_storage: db 0        ; Store drive ID here (at 0x7DFD)
dw 0xAA55                       ; Boot signature


start:
    cli                     ; Disable interrupts for setup
    
    ; 1. Setup Segments and Stack
    ; CRITICAL FIX: Set DS to 0x7C0 so DS:SI=0x7C0:0x0000 correctly
    ; addresses the DAP at physical 0x7C00.
    mov ax, BOOT_SEGMENT_BASE ; AX = 0x07C0
    mov ds, ax              ; DS=0x7C0 (for accessing DAP and local data)
    
    ; ES is used for the destination buffer (0x1000:0x0000)
    xor ax, ax              
    mov es, ax              ; ES=0x0000 (Set later for buffer)
    
    ; Set up Stack (SS:SP = 0x7C0:0x7C00) - Stack must be within current segment
    ; Use 0x7C0:0x7C00. Physical: (0x7C0 * 10h) + 0x7C00 = 0xF800 (high enough)
    mov ss, ax              ; SS=0x7C0
    mov sp, 0x7C00          ; Stack starts high
    
    ; Save boot drive ID (DL is set by BIOS upon boot)
    ; We need to use the full physical address here since DS is not 0
    mov word [0x7DFD], dx   ; Save DL at 0x7DFD (using DS=0x7C0 segment)
    
    sti                     ; Re-enable interrupts

    ; 2. Execute LBA Read (INT 0x13 / AH=0x42)
    
    ; Setup Buffer Address (ES:BX = 0x0100:0x0000 -> 0x1000 physical)
    mov ax, KERNEL_LOAD_SEGMENT
    mov es, ax
    mov bx, KERNEL_LOAD_OFFSET
    
    ; Setup DS:SI to point to the DAP structure (DS=0x7C0, SI=0x0000)
    xor si, si              ; SI = 0x0000
    
    mov ah, 0x42            ; Extended Read Sectors
    
    ; Use the actual boot drive ID from the saved location
    mov dl, [0x7DFD]        ; Load drive ID from the address *within* DS segment
    
    int 0x13                ; Execute disk read
    
    ; Check for error
    jnc jump_to_stage2      ; If carry flag is clear, success!
    
    ; Read failed: Print error 'E' and error code in AH
    jmp disk_read_failed
    
; --------------------------------------
; JUMP LOGIC
; --------------------------------------

jump_to_stage2:
    
    ; 3. Final Segment Setup for Stage 2 (CRITICAL)
    ; Set DS and ES to the target segment for kernel.asm's context.
    mov ax, KERNEL_LOAD_SEGMENT ; AX = 0x0100
    mov ds, ax              
    mov es, ax              

    ; 4. Jump to the loaded kernel (Stage 2)
    push KERNEL_LOAD_SEGMENT ; Pushes 0x0100 (CS) onto the stack
    push KERNEL_LOAD_OFFSET  ; Pushes 0x0000 (IP) onto the stack
    retf                     ; Far Return/Jump to 0x0100:0x0000

; --------------------------------------
; ERROR HANDLING
; --------------------------------------
disk_read_failed:
    ; Need to adjust DS back to 0 so we can print to VGA text mode (0xB8000)
    ; and use the print routines correctly (if they relied on a specific DS)
    push ds
    mov ax, 0x0000
    mov ds, ax
    
    mov ch, ah              ; Save AH (error code) into CH

    mov ah, 0x0E            ; Print character function
    mov al, 'E'
    call print_char         ; Print 'E'
    
    ; Print AH error code (first hex digit)
    mov al, ch              ; Get error code back into AL
    shr al, 4               ; Shift high nibble to low nibble
    call print_hex_digit
    
    ; Print AH error code (second hex digit)
    mov al, ch              ; Get error code back into AL
    call print_hex_digit
    
    pop ds                  ; Restore DS
    cli                     ; Disable interrupts
    hlt                     ; Halt
    jmp $                   ; Infinite loop

; --------------------------------------
; 16-bit UTILITIES
; --------------------------------------

; Prints the character currently in AL (assumes AH=0x0E)
print_char:
    int 0x10
    ret

; Converts AL (0-15) to ASCII hex digit and prints it
print_hex_digit:
    push ax                 ; Save AX
    push bx
    
    and al, 0x0F            ; Isolate the low 4 bits (the digit)
    cmp al, 0x09
    jbe is_digit
    add al, 0x07            ; If 10-15, add 7 (A-F)
is_digit:
    add al, 0x30            ; Add ASCII '0'
    mov ah, 0x0E            ; Set print function code
    call print_char
    
    pop bx
    pop ax
    ret