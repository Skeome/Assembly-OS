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
KERNEL_START_LBA_32     equ 0x29    ; LBA 41 (Kernel32 starts here)
BOOT_DRIVE_ADDRESS      equ 0x7DFD  ; Drive ID storage (from boot.asm, DS=0)
TEMP_STACK_PTR          equ 0x7000  ; Dedicated stack for BIOS calls (below 0x7C00)

; --- Disk Address Packet (DAP) Structure for AH=0x42 ---
dap_packet:
dap_size:           db 0x10         
dap_reserved:       db 0x00
dap_sectors:        dw KERNEL32_SECTORS_TO_READ 
dap_buffer_offset:  dw 0x0000                     
dap_buffer_segment: dw KERNEL32_LOAD_SEGMENT    
dap_lba_low:        dd KERNEL_START_LBA_32      
dap_lba_high:       dd 0x00000000

disk_error_code:    db 0

start:
    cli                         ; Disable interrupts while setting up
    
    ; --- 1. Setup Segments and Stack ---
    xor ax, ax                  
    mov ds, ax                  ; DS=0: Required for reading BOOT_DRIVE_ADDRESS (0x7DFD)
    mov es, ax                  
    mov ss, ax                  
    mov sp, 0x9000              ; Set main stack pointer (0x9000 physical)
    
    sti                         ; Re-enable interrupts
    
    ; --- 2. CLEAR SCREEN & DISPLAY 16-BIT WELCOME ---
    
    ; Clear screen using BIOS INT 0x10, AH=0x06 (Scroll up function)
    mov ah, 0x06 ; Scroll up function
    mov al, 0x00 ; Scroll whole window
    mov bh, 0x07 ; Attribute (White on Black)
    mov cx, 0x0000 ; Start at row 0, column 0
    mov dx, 0x184F ; End at row 24, column 79 (80x25)
    int 0x10 ; Screen is now cleared.

    ; Display 16-bit welcome using direct VGA memory write (DS must be B800)
    push ds ; Save DS=0
    mov ax, 0xB800
    mov ds, ax ; DS = 0xB800 (VGA Text Mode)
    
    mov si, msg_16bit_welcome_offset ; SI holds the offset of the message (relative to DS=0)
    mov di, 0x00
.print_loop:
    lodsb                   
    cmp al, 0               
    je .print_done
    mov ah, 0x0F ; Attribute: White on Black
    stosw
    jmp .print_loop
.print_done:
    pop ds ; Restore DS=0

    ; Load drive ID for disk calls (DL is needed for INT 0x13)
    mov dl, [BOOT_DRIVE_ADDRESS] 

    ; --- 3. Load KERNEL32.BIN (Stage 3) ---
    
    ; Setup Buffer Address (ES:BX = 0x1000:0x0000 -> 0x10000 physical)
    mov ax, KERNEL32_LOAD_SEGMENT
    mov es, ax
    mov bx, 0x0000
    
    ; CRITICAL: Set DS to our segment (0x0100) to access DAP internal data.
    push ds ; Save DS=0
    mov ax, 0x0100
    mov ds, ax 
    mov si, dap_packet          ; DS:SI points to DAP

    ; Attempt Read (with retries) - DL is already set
    mov di, 3                       ; Retry count
.retry_read:
    push sp                         
    mov sp, TEMP_STACK_PTR          ; Switch to dedicated stack 
    
    pusha                           
    
    mov ah, 0x42                    ; Extended Read Sectors
    
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
    
    ; --- 4. Enable A20 Gate ---
    mov ax, 0x2402              ; A20 enable function (int 0x15)
    int 0x15                    
    jc disk_error               ; Jump if A20 failed

    ; --- 5. Load GDT ---
    ; Re-set DS to our load segment (0x100) to access GDT data internally.
    mov ax, 0x0100
    mov ds, ax
    
    ; CRITICAL GDT ADDRESS FIX: Update GDT descriptor base address
    mov ebx, gdt_start          ; Get GDT label offset
    add ebx, 0x1000             ; Add the load address 0x1000 
    mov dword [gdt_descriptor + 2], ebx 

    lgdt [gdt_descriptor]

    ; --- 6. Set up Data Segment Registers with new selector (0x10) ---
    mov ax, 0x10    ; The Data Segment Selector
    mov ds, ax      
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax      

    ; --- 7. Switch to Protected Mode ---
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    
    ; --- JUMP TO 32-BIT KERNEL (Stage 3) ---
    push dword KERNEL32_JUMP_ADDRESS ; Pushes 0x10000 (EIP)
    push dword 0x08                  ; Pushes 0x08 (CS Selector)
    retf                             
    
; --------------------------------------
; Error Handling and Data
; --------------------------------------

; The print utilities have been removed to keep this file minimal.
; The error handler simply uses BIOS INT 0x10.
disk_error:
    ; Ensure DS=0 for error message string access.
    pop ds 
    xor ax, ax
    mov ds, ax
    
    mov si, msg_disk_error_generic 
.print_err:
    mov ah, 0x0E
    lodsb                   
    cmp al, 0               
    je .halt
    int 0x10
    jmp .print_err
.halt:
    cli
    hlt
    jmp $

; Calculate offset of the welcome message for DS=B800 access
msg_16bit_welcome_offset equ msg_16bit_welcome - 0x1000
msg_16bit_welcome:  db 'Welcome to 16-bit kernel mode. Loading 32-bit core...', 0
msg_disk_error_generic: db ' Disk I/O error! Halting.', 0
; --- Global Descriptor Table (GDT) ---
gdt_start:
    ; 0x00: Null descriptor
    dq 0x0

    ; 0x08: Code segment descriptor (32-bit, Ring 0)
    dw 0xFFFF ; Limit
    dw 0x0000 ; Base
    db 0x00
    db 0b10011010 ; Access
    db 0b11001111 ; Flags + Limit
    db 0x00

    ; 0x10: Data segment descriptor (32-bit, Ring 0)
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 0b10010010 ; Access
    db 0b11001111
    db 0x00

gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1  ; Limit
    dd gdt_start                ; Base address (UPDATED AT RUNTIME)

; --- Padding ---
times 20480 - ($ - $$) db 0 ; Pad to 20KB (40 sectors)