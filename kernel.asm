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
KERNEL_START_LBA_32     equ 0x29    ; LBA 41 (Kernel32 starts here - starting point for CHS read)
BOOT_DRIVE_ADDRESS      equ 0x7DFD  ; Drive ID storage (from boot.asm, DS=0)
TEMP_STACK_PTR          equ 0x7000  ; Dedicated stack for BIOS calls (below 0x7C00)

; --- Disk Address Packet (DAP) Structure for AH=0x42 ---
; NOTE: DAP structure is kept for reference but NO LONGER USED in read routine.
dap_packet:
dap_size:           db 0x10         
dap_reserved:       db 0x00
dap_sectors:        dw KERNEL32_SECTORS_TO_READ 
dap_buffer_offset:  dw 0x0000                     
dap_buffer_segment: dw KERNEL32_LOAD_SEGMENT    
dap_lba_low:        dd KERNEL_START_LBA_32      
dap_lba_high:       dd 0x00000000

disk_error_code:    db 0
current_lba:        dw KERNEL_START_LBA_32 ; Tracks the next LBA to read
sectors_left:       dw KERNEL32_SECTORS_TO_READ ; Tracks sectors remaining

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

    ; Load drive ID for disk calls (DL is needed for INT 0x13)
    mov dl, [BOOT_DRIVE_ADDRESS] 
    
    ; --- FIX: Set DS to kernel segment (0x100) for source string, ES to B800 for video target ---
    push ds ; Save DS=0
    mov ax, 0x0100
    mov ds, ax ; DS = 0x0100 (Physical address 0x1000, for accessing kernel data)
    
    mov ax, 0xB800
    mov es, ax ; ES = 0xB800 (VGA Text Mode segment)

    mov si, msg_16bit_welcome_offset ; SI holds the offset of the message (relative to 0x1000)
    mov di, 0x00 ; DI = offset 0 (top-left)
    
.print_loop:
    lodsb                   
    cmp al, 0               
    je .print_done
    mov ah, 0x0F ; Attribute: White on Black
    stosw
    jmp .print_loop
.print_done:
    pop ds ; Restore DS=0 (Needed for [BOOT_DRIVE_ADDRESS] at the very top of memory)
    
    ; --- 3. Load KERNEL32.BIN (Stage 3) using AH=0x02 (Legacy CHS Read) ---
    
    ; Setup Buffer Address (ES:BX = 0x1000:0x0000 -> 0x10000 physical)
    mov ax, KERNEL32_LOAD_SEGMENT
    mov es, ax
    mov bx, 0x0000          ; Destination offset starts at 0x0000 (0x10000 physical)
    
    mov cx, 3               ; Retry count for the whole load loop
.load_loop_retry:
    push cx
    
    ; Initialize loop variables
    mov word [current_lba], KERNEL_START_LBA_32
    mov word [sectors_left], KERNEL32_SECTORS_TO_READ
    
.read_sector_loop:
    ; --- LBA to CHS Conversion (LBA -> CH:Cylinder, DH:Head, CL:Sector) ---
    mov ax, [current_lba]
    xor dx, dx                  ; DX:AX = LBA
    mov bl, 18                  ; Sectors Per Track (SPT)
    div bl                      ; AL = (C * 2 + H), AH = (S - 1)
    
    push ax                     ; Save AX, which holds (C*2+H) and (S-1)
    
    mov cl, 2                   ; Heads Per Cylinder (HPC)
    div cl                      ; AL = Cylinder (C), AH = Head (H)
    
    mov ch, al                  ; CH = Cylinder
    mov dh, ah                  ; DH = Head
    
    pop ax                      ; Restore AX. AL=(C*2+H), AH=(S-1)
    
    mov al, ah                  ; AL = (S-1)
    inc al                      ; AL = Sector (S), 1-18
    mov cl, al                  ; CL = Sector (1-18)
    
    ; Clear AH and ensure AL=1 for AH=0x02 call
    mov ah, 0x02                ; Function 0x02: Read Sector
    mov al, 1                   ; Read 1 sector (THIS IS CRITICAL)
    
    pusha                       ; Save all general registers
    
    int 0x13
    
    jnc .sector_read_success    ; If CF is clear, success
    
    ; --- Read failure - Reset and Retry ---
    popa                        ; Restore state before reset attempt
    pusha
    mov ah, 0x00                ; Reset Disk System
    mov dl, [BOOT_DRIVE_ADDRESS]
    int 0x13
    popa
    
    pop cx                      ; Restore main retry counter
    loop .load_loop_retry       ; Retry the entire load process
    
    ; All retries failed
    jmp disk_error

.sector_read_success:
    popa                        ; Restore saved registers
    
    ; Advance counters
    inc word [current_lba]          ; LBA += 1
    add bx, 0x200                   ; Dest offset += 512 bytes (1 sector)
    
    ; Check if done
    dec word [sectors_left]
    jnz .read_sector_loop           ; Read next sector if sectors_left > 0
    
    ; Success! Kernel loaded.
    jmp .read_success               ; Jump to A20 activation

.read_success:
    pop ds ; Restore DS to 0x0000 (Saved before the load loop start)
    
    ; --- 4. ENABLE A20 GATE (FAST PORT 0x92 METHOD) ---
    ; This method is the most reliable for virtual environments.
    cli
    
    ; Read the current status of port 0x92
    in al, 0x92
    
    ; OR the A20 bit (bit 1) to enable it
    or al, 0x02 
    
    ; Write the byte back to port 0x92
    out 0x92, al
    
    ; The code proceeds immediately to GDT setup.

    jmp a20_check_success ; Skip subroutines

a20_check_success:
    ; --- 5. Load GDT (Fixed previously, using correct physical address) ---
    ; Re-set DS to our load segment (0x100) to access GDT data internally.
    mov ax, 0x0100
    mov ds, ax
    
    ; CRITICAL GDT ADDRESS FIX: 
    mov ebx, gdt_start          ; EBX now holds the correct physical GDT base address
    mov dword [gdt_descriptor + 2], ebx ; Update the GDT descriptor base address

    lgdt [gdt_descriptor]

    ; --- 6. Set up Data Segment Registers with new selector (0x10) ---
    ; Removed pre-PM loads.
    
    ; --- 7. Switch to Protected Mode ---
    cli ; Ensure interrupts are disabled for transition
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    ; --- CRITICAL FIX: Use a far jump for 32-bit switch ---
    jmp 0x08:KERNEL32_JUMP_ADDRESS ; Far jump to 0x08:0x10000
    
; --------------------------------------
; Error Handling and Data
; --------------------------------------

; The print utilities have been removed to keep this file minimal.
; The error handler simply uses BIOS INT 0x10.
disk_error:
    ; Clear screen before printing fatal error
    mov ah, 0x06 ; Scroll up function
    mov al, 0x00 ; Scroll whole window
    mov bh, 0x07 ; Attribute (White on Black)
    mov cx, 0x0000 ; Start at row 0, column 0
    mov dx, 0x184F ; End at row 24, column 79 (80x25)
    int 0x10 ; Screen is now cleared.

    ; Ensure DS=0 for error message string access.
    ; pop ds is removed as it's not needed here and can corrupt the stack depth
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