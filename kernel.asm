; kernel.asm
;
; Stage 2 bootloader (16-bit Real Mode)
; Loaded by stage 1 at 0x1000.
; Job: Load kernel32.asm, set up GDT, and switch to 32-bit Protected Mode.
;

[BITS 16]
[ORG 0x1000] ; We are loaded at physical address 0x1000

KERNEL32_LOAD_SEGMENT   equ 0x1000  ; Segment to load kernel32.bin (0x1000 * 16 = 0x10000)
KERNEL32_JUMP_ADDRESS   equ 0x10000 ; 32-bit physical address to jump to
KERNEL32_SECTORS_TO_READ equ 256    ; Load 128 KB (256 sectors)
KERNEL_START_LBA_32     equ 0x29    ; LBA 41 (40 sectors for Stage 2 + 1 sector for boot = LBA 41)
BOOT_DRIVE_ADDRESS      equ 0x7DFD  ; Drive ID storage (from boot.asm)
TEMP_STACK_PTR          equ 0x7000  ; Dedicated stack for BIOS calls

; Disk Address Packet (DAP) Structure for AH=0x42
dap_packet:
dap_size:           db 0x10
dap_reserved:       db 0x00
dap_sectors:        dw 0x0000
dap_buffer_offset:  dw 0x0000
dap_buffer_segment: dw 0x0000
dap_lba_low:        dd 0x00000000
dap_lba_high:       dd 0x00000000

; --- Data for read loop (DAP) ---
sectors_to_read:    dw 0
sectors_this_read:  db 0
disk_error_code:    db 0
current_lba:        dd 0          ; LBA counter

; --- UTILITIES (Kept here for compilation consistency) ---
print_char:                 ; Routine to print a character in AL
    mov ah, 0x0E
    int 0x10
    ret

; Utility to print AH as a single hex digit (0-F) for debugging
print_hex_digit:
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
    cli                         ; Disable interrupts first
    
    ; --- Set up the stack and segment registers (CRITICAL FIRST STEP) ---
    xor ax, ax                  
    mov ds, ax                  ; DS=0 for accessing DAP/GDT (Memory below 64KB)
    mov es, ax                  ; ES=0 initially
    mov ss, ax                  ; SS=0
    mov sp, 0x9000              ; Set main stack pointer (0x9000)
    
    sti                         ; Re-enable interrupts

    ; --- CHECKPOINT 1: Kernel Start (Now safe after stack/segments initialized) ---
    mov ah, 0x0E
    mov al, '1'
    int 0x10
    
    ; Load drive ID 
    mov dl, [BOOT_DRIVE_ADDRESS] ; Load drive ID from 0x7DFD
    
    ; --- CHECKPOINT 2: Read Setup Complete ---
    mov al, '2'
    call print_char
    
    ; --- 3. Initialize LBA Read Parameters for KERNEL32 (Load from LBA 41) ---
    mov word [sectors_to_read], KERNEL32_SECTORS_TO_READ
    mov dword [current_lba], KERNEL_START_LBA_32 
    
    mov ax, KERNEL32_LOAD_SEGMENT ; ES = 0x1000 (Load to 0x10000)
    mov es, ax
    mov bx, 0x0000                  ; Offset 0
    
.read_loop:
    cmp word [sectors_to_read], 0
    jz .a20_stage                   ; All sectors read, jump to A20

    ; --- CHECKPOINT 3: Reading Block ---
    mov al, '3'
    call print_char

    ; Calculate sectors to read (read up to remaining sectors)
    mov ax, [sectors_to_read]
    mov byte [dap_sectors], al      
    mov byte [sectors_this_read], al 

    ; Set DAP parameters
    mov word [dap_buffer_offset], bx 
    mov word [dap_buffer_segment], es 
    
    mov ax, [current_lba]           ; LBA Low
    mov [dap_lba_low], ax
    mov ax, [current_lba + 2]       ; LBA High
    mov [dap_lba_low + 2], ax
    
    ; --- Attempt Read (with retries) ---
    mov di, 3                       ; Retry count
.retry_read:
    push sp                         ; Save SP
    mov sp, TEMP_STACK_PTR          ; Switch to dedicated stack
    pusha                           
    
    mov ah, 0x42                    ; Extended Read Sectors
    mov dl, [BOOT_DRIVE_ADDRESS]    
    mov si, dap_packet              ; DS:SI points to DAP (DS=0)
    
    int 0x13
    
    mov byte [disk_error_code], ah
    
    popa                            
    pop sp                          ; Restore SP
    
    jnc .read_success               ; Success!
    
    ; Read failed, reset disk and retry
    pusha
    mov dl, [BOOT_DRIVE_ADDRESS]    
    mov ah, 0x00
    int 0x13
    popa
    
    dec di                          
    jnz .retry_read
    
    ; --- Read failed after all retries ---
    jmp disk_error                  


.read_success:
    ; --- CHECKPOINT 4: Successful Read ---
    mov al, '4'
    call print_char
    
    ; Update counters
    mov al, [sectors_this_read]
    xor ah, ah                      ; AX = sectors read
    
    sub [sectors_to_read], ax       
    
    ; Update current_lba (32-bit addition)
    mov dx, word [current_lba]      
    add dx, ax                      
    mov word [current_lba], dx      
    
    mov dx, word [current_lba+2]    
    adc dx, 0                       
    mov word [current_lba+2], dx    
    
    ; Calculate new paragraph offset for ES
    mov cx, 32                      ; 512 bytes / 16 bytes per paragraph
    mul cx                          ; AX = paragraph offset
    
    mov bx, es                      
    add bx, ax                      
    mov es, bx                      
    
    jmp .read_loop                  

.a20_stage:
    ; --- CHECKPOINT 5: Pre-Protected Mode Setup ---
    mov al, '5'
    call print_char

    ; --- Enable A20 Gate ---
    mov ax, 0x2402              
    int 0x15                    
    jc disk_error               

    ; --- CHECKPOINT 6: A20 Enabled ---
    mov al, '6'
    call print_char

    ; --- Load GDT ---
    lgdt [gdt_descriptor]

    ; --- Set up Data Segment Registers to be valid when switching modes ---
    mov ax, 0x10  ; The Data Segment Selector
    mov ds, ax    
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax    ; CRITICAL: SS must point to a valid segment before CR0 bit 0 is set

    ; --- CHECKPOINT 7: GDT Loaded, Segments Initialized ---
    mov al, '7'
    call print_char
    
    ; --- Switch to Protected Mode ---
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    
    ; --- JUMP TO 32-BIT KERNEL ---
    ; This is the final 16-bit instruction, jumping to 0x08:0x10000
    push dword KERNEL32_JUMP_ADDRESS ; Pushes 0x10000 (EIP)
    push dword 0x08                  ; Pushes 0x08 (CS Selector)
    retf                             ; Atomically loads EIP and CS, executing Stage 3
    
; --------------------------------------
; Error Handling and Data
; --------------------------------------

disk_error:
    mov ah, 0x0E
    mov al, 'E'
    int 0x10
    
    mov al, [disk_error_code]
    call print_hex_digit
    
    mov si, msg_disk_error_generic 
    call print_string
    cli
    hlt
    jmp $

msg_disk_error_generic: db 'Disk I/O error during stage 2 load! Halting.', 0

; --- Global Descriptor Table (GDT) ---
gdt_start:
    ; Null descriptor 0x00
    dq 0x0

    ; Code segment descriptor (selector 0x08)
    dw 0xFFFF                   ; Limit (low)
    dw 0x0000                   ; Base (low)
    db 0x00                     ; Base (mid)
    db 0b10011010               ; Access (Code, Present, Ring 0, Readable)
    db 0b11001111               ; Flags (Granularity=4K, 32-bit) + Limit (high)
    db 0x00                     ; Base (high)

    ; Data segment descriptor (selector 0x10)
    dw 0xFFFF                   
    dw 0x0000                   
    db 0x00                     
    db 0b10010010               ; Access (Data, Present, Ring 0, Writable)
    db 0b11001111               
    db 0x00                     

gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1  ; Limit
    dd gdt_start                ; Base address (Physical 0x1000 + offset to gdt_start)

; --- Padding ---
times 20480 - ($ - $$) db 0