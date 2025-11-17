; kernel.asm
;
; Stage 2 bootloader (16-bit Real Mode)
; Loaded by stage 1 at physical address 0x1000.
; Job: Load kernel32.asm, set up GDT, enable A20, and switch to 32-bit Protected Mode.
;

[BITS 16]
[ORG 0x1000] ; We are loaded at physical address 0x1000

; --- Constants for Stage 3 Kernel ---
KERNEL32_LOAD_SEGMENT   equ 0x1000  ; Segment to load kernel32.bin (0x1000 * 16 = 0x10000 physical)
KERNEL32_JUMP_ADDRESS   equ 0x10000 ; 32-bit physical address to jump to
KERNEL32_SECTORS_TO_READ equ 256    ; Load 128 KB (256 sectors)
KERNEL_START_LBA_32     equ 0x29    ; LBA 41 (Sectors 2-41 are kernel.asm, so kernel32 starts at LBA 41)
BOOT_DRIVE_ADDRESS      equ 0x7DFD  ; Drive ID storage (from boot.asm at 0x7C00 + 0x1FD)
TEMP_STACK_PTR          equ 0x7000  ; Dedicated stack for BIOS calls (below 0x7C00)

; --- Disk Address Packet (DAP) Structure for AH=0x42 ---
dap_packet:
dap_size:           db 0x10         ; Size of DAP (16 bytes)
dap_reserved:       db 0x00
dap_sectors:        dw KERNEL32_SECTORS_TO_READ ; Sectors to read (256)
dap_buffer_offset:  dw 0x0000                     ; Load offset (0x0000)
dap_buffer_segment: dw KERNEL32_LOAD_SEGMENT    ; Load segment (0x1000)
dap_lba_low:        dd KERNEL_START_LBA_32      ; LBA Start (41)
dap_lba_high:       dd 0x00000000


; --- UTILITIES (Minimal 16-bit debugging/printing) ---
disk_error_code:    db 0

print_char:                 ; Routine to print a character in AL
    mov ah, 0x0E
    int 0x10
    ret

print_hex_digit:            ; Utility to print AH as a single hex digit (0-F) for debugging
    push ax
    push bx
    push cx
    
    mov al, ah              ; Get AH (error code) into AL
    and al, 0x0F
    cmp al, 0x09
    jbe .is_digit
    add al, 0x07   
.is_digit:
    add al, 0x30   
    call print_char
    
    pop cx
    pop bx
    pop ax
    ret
    
print_string:
    pusha
.loop_str:
    lodsb                   
    cmp al, 0               
    je .done_str
    call print_char
    jmp .loop_str
.done_str:
    popa
    ret
    
start:
    cli                         ; Disable interrupts while setting up
    
    ; --- 1. Set up Segments and Stack (CRITICAL FIX) ---
    ; DS, ES, SS must be set to 0. We'll rely on the BIOS to keep CS pointing to 0x1000.
    xor ax, ax                  
    mov ds, ax                  ; DS=0: Required to read BOOT_DRIVE_ADDRESS (0x7DFD)
    mov es, ax                  ; ES=0: Temporary, will be reset for load buffer
    mov ss, ax                  ; SS=0
    mov sp, 0x9000              ; Set main stack pointer (0x9000 physical)
    
    sti                         ; Re-enable interrupts

    ; --- CHECKPOINT 1: Kernel Start ---
    mov ah, 0x0E
    mov al, '1'
    int 0x10
    
    ; Load drive ID for disk calls
    mov dl, [BOOT_DRIVE_ADDRESS] ; Load drive ID from 0x7DFD (DS=0 is correct)

    ; --- 2. Load KERNEL32.BIN (Stage 3) in one go ---
    
    ; Setup Buffer Address for disk read (ES:BX = 0x1000:0x0000 -> 0x10000 physical)
    mov ax, KERNEL32_LOAD_SEGMENT
    mov es, ax
    mov bx, 0x0000
    
    ; Setup DS:SI to point to the DAP structure.
    ; CRITICAL FIX: Since DAP is at ORG 0x1000, we must temporarily set DS to 0x0100.
    push ds ; Save DS=0
    mov ax, 0x0100
    mov ds, ax ; DS = 0x0100 (Now DS:dap_packet points to 0x1000 + offset)

    mov si, dap_packet          ; SI = offset of DAP within the 0x100 segment
    
    ; Attempt Read (with retries) - DL is already set
    mov di, 3                       ; Retry count
.retry_read:
    push sp                         ; Save original SP (0x9000)
    mov sp, TEMP_STACK_PTR          ; Switch to dedicated stack (0x7000)
    
    pusha                           ; Save general registers (AX is still needed for AH=0x42)
    
    mov ah, 0x42                    ; Extended Read Sectors (INT 0x13)
    
    int 0x13
    
    mov byte [disk_error_code], ah
    
    popa                            
    pop sp                          ; Restore SP
    
    jnc .read_success               ; Success!
    
    ; Read failed, reset disk and retry
    pusha
    mov ah, 0x00                    ; Reset Disk System
    int 0x13
    popa
    
    dec di                          
    jnz .retry_read
    
    ; --- Read failed after all retries ---
    jmp disk_error                  


.read_success:
    pop ds ; Restore DS to 0x0000
    
    ; --- CHECKPOINT 2: Successful Load ---
    mov ah, 0x0E
    mov al, '2'
    int 0x10
    
    ; --- 3. Enable A20 Gate ---
    mov ax, 0x2402              ; A20 enable function (int 0x15)
    int 0x15                    
    jc disk_error               ; Jump if A20 failed (unlikely in modern VMs)

    ; --- CHECKPOINT 3: A20 Enabled ---
    mov ah, 0x0E
    mov al, '3'
    int 0x10

    ; --- 4. Load GDT ---
    ; Re-set DS to our load segment (0x100) to access GDT data internally.
    mov ax, 0x0100
    mov ds, ax
    
    ; CRITICAL GDT ADDRESS FIX: 
    ; The GDT definition is physically at 0x1000 + offset. We must explicitly
    ; set the base address field (gdt_descriptor + 2) to this physical address.
    mov ebx, gdt_start          ; Get GDT label offset (offset from 0x1000)
    add ebx, 0x1000             ; Add the load address 0x1000 (Physical address)
    mov dword [gdt_descriptor + 2], ebx ; Update the 32-bit base address field of the descriptor

    lgdt [gdt_descriptor]

    ; --- CHECKPOINT 4: GDT Loaded ---
    mov ah, 0x0E
    mov al, '4'
    int 0x10
    
    ; --- 5. Set up Data Segment Registers with new selector (0x10) ---
    mov ax, 0x10    ; The Data Segment Selector
    mov ds, ax      
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax      ; CRITICAL: SS must point to a valid segment before CR0 bit 0 is set

    ; --- CHECKPOINT 5: Segments Initialized ---
    mov al, '5'
    int 0x10
    
    ; --- 6. Switch to Protected Mode ---
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    
    ; --- JUMP TO 32-BIT KERNEL (Stage 3) ---
    ; This is the final 16-bit instruction.
    ; Far jump loads 0x08 into CS and 0x10000 into EIP atomically.
    push dword KERNEL32_JUMP_ADDRESS ; Pushes 0x10000 (EIP)
    push dword 0x08                  ; Pushes 0x08 (CS Selector)
    retf                             
    
; --------------------------------------
; Error Handling and Data
; --------------------------------------

disk_error:
    ; Need to ensure DS=0 for error message string access and video printing.
    push ds
    xor ax, ax
    mov ds, ax
    
    mov ah, 0x0E
    mov al, 'E'
    int 0x10
    
    ; Print the error code
    mov al, [disk_error_code]
    mov ah, al
    shr ah, 4
    call print_hex_digit
    mov al, [disk_error_code]
    call print_hex_digit

    mov si, msg_disk_error_generic 
    call print_string
    
    pop ds ; Restore DS
    cli
    hlt
    jmp $

msg_disk_error_generic: db ' Disk I/O error! Halting.', 0
; --- Global Descriptor Table (GDT) ---
gdt_start:
    ; 0x00: Null descriptor (Mandatory)
    dq 0x0

    ; 0x08: Code segment descriptor (32-bit, Ring 0)
    dw 0xFFFF                   ; Limit (low) 0-4GB
    dw 0x0000                   ; Base (low)
    db 0x00                     ; Base (mid)
    db 0b10011010               ; Access (Present, DPL=0, Code, Readable)
    db 0b11001111               ; Flags (Granularity=4K, 32-bit) + Limit (high)
    db 0x00                     ; Base (high)

    ; 0x10: Data segment descriptor (32-bit, Ring 0)
    dw 0xFFFF                   
    dw 0x0000                   
    db 0x00                     
    db 0b10010010               ; Access (Present, DPL=0, Data, Writable)
    db 0b11001111               
    db 0x00                     

gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1  ; Limit (size of GDT - 1)
    dd gdt_start                ; Base address (THIS IS UPDATED AT RUNTIME)

; --- Padding ---
times 20480 - ($ - $$) db 0 ; Pad to 20KB (40 sectors)