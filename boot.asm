; boot.asm - Stage 1 Bootloader
; Job: Load kernel.bin (Stage 2) into memory at 0x1000 and execute it.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

start:
    cli                     ; Disable interrupts
    
    ; --- 1. Setup Segments and Stack (Use DS=0 for simplicity, assuming BIOS supports) ---
    xor ax, ax              ; AX = 0
    mov ds, ax              ; DS = 0 (Access local data via absolute offset 0x7C00)
    mov es, ax              ; ES = 0 (Will be reset for load buffer)
    mov ss, ax              ; SS = 0
    mov sp, 0x7C00          ; Stack grows down from 0x7C00


    mov [boot_drive_id], dl ; Store boot drive number for later use

    sti                     ; Re-enable interrupts
    
    ; --- 2. Reset the disk controller (with retries) ---
    mov cx, 3  ; Number of retries
.reset_loop:
    push cx
    mov ah, 0x00            ; Function 0x00: Reset Disk System
    mov dl, [boot_drive_id] ; DL = Drive number (from bootloader)
    int 0x13
    jnc .load_stage2        ; If carry flag is clear, success! Jump to load.
    pop cx
    loop .reset_loop        ; If carry flag was set (error), try again.
    
    jmp disk_error          ; If all retries fail, then show error.

.load_stage2:

    pop cx ; Discard the previous CX pushed before the .reset_loop

    ; Load the kernel from disk (with retries)
    mov cx, 3  ; Reset retry counter for the read operation
.read_loop:
    push cx
    
    mov ax, 0x0100          ; AX = 0x0100
    mov es, ax              ; ES = 0x0100 (Destination segment: 0x1000)
    mov bx, 0x0000          ; BX = 0x0000 (Destination offset)

    mov ah, 0x02            ; Function 0x02: Read Sectors
    mov al, 40              ; AL = 40 (Read 40 sectors - matches 20KB kernel size)
    mov ch, 0               ; CH = Cylinder 0
    mov cl, 2               ; CL = Sector 2 (Sector 1 is the boot sector, start reading at 2)
    mov dh, 0               ; DH = Head 0
    mov dl, [boot_drive_id] ; DL = Drive number (from bootloader)
    
    int 0x13
    jnc .jump_to_stage2     ; If carry flag is clear, success!
    
    ; If read failed, reset the disk system again before retrying
    push ax                 ; Save AX
    mov ah, 0x00            ; Function 0x00: Reset Disk System
    mov dl, [boot_drive_id] ; DL = Drive number (from bootloader)
    int 0x13
    pop ax                  ; Restore AX
    
    pop cx
    loop .read_loop         ; Try reading again
    
    mov [disk_error_code], ah ; Store error code
    jmp disk_error

.jump_to_stage2:
    ; --- 4. Prepare Segments for Stage 2 (Must be correct for Stage 2's ORG) ---
    mov ax, 0x0100          ; AX = 0x0100
    mov ds, ax              ; DS = 0x0100
    mov es, ax              ; ES = 0x0100
    
    ; --- 5. Jump to the loaded kernel ---
    jmp 0x0100:0x0000       ; Far jump to 0x0100:0x0000

; --- Enhanced Error Handling ---
disk_error:
    ; DS is 0x0000, needed for accessing data below 0x7C00
    mov bl, [disk_error_code] ; Load error code into BL (e.g., 01h, 04h, etc.)
    
    mov si, error_table       ; SI points to the start of the error lookup table
    
.lookup_loop:
    cmp byte [si], 0xFF       ; Check for the end-of-table marker (0xFF)
    je .print_generic_error   ; If found, no specific message exists.

    cmp bl, [si]              ; Compare saved error code (BL) with the code in the table
    je .found_error           ; If they match, we found the message!

    add si, 3                 ; Skip to the next entry: 1 byte code + 2 byte pointer
    jmp .lookup_loop

.found_error:
    inc si                    ; Move SI past the error code byte
    mov si, [si]              ; Load the 2-byte pointer (offset) into SI
    jmp print_string

.print_generic_error:
    mov si, msg_disk_error_generic
    jmp print_string

print_string:
    mov ah, 0x0E            ; BIOS teletype output function
.loop:
    lodsb                   ; Load character from [DS:SI] into AL (DS=0)
    cmp al, 0
    je .halt
    int 0x10                ; Call BIOS video interrupt
    jmp .loop
.halt:
    cli
    hlt
    
; --- Data ---
disk_error_code: db 0         ; Variable to store AH error code

; Error Lookup Table (Code: 1 byte, Pointer: 2 bytes)
error_table:
    db 0x01  ; Code 01h: Invalid Command/Parameter
    dw msg_err_01
    db 0x02  ; Code 02h: Address Mark Not Found
    dw msg_err_02
    db 0x04  ; Code 04h: Sector Not Found/Read Error
    dw msg_err_04
    db 0x80  ; Code 80h: Drive Not Ready/Timeout
    dw msg_err_80
    db 0xFF  ; End of table marker

; Error Message Strings
msg_err_01: db 'Disk Error 01h: Invalid Command/Parameter.', 0
msg_err_02: db 'Disk Error 02h: Address Mark Not Found.', 0
msg_err_04: db 'Disk Error 04h: Sector Not Found/Read Error.', 0
msg_err_80: db 'Disk Error 80h: Drive Not Ready/Timeout. Check disk connection.', 0
msg_disk_error_generic: db 'Disk read error! Unknown code or unlisted error.', 0

boot_drive_id: db 0         ; Reserve 1 byte for boot drive ID

; Padding to 512 bytes total.
times 512-($-$$) db 0
; Mandatory boot signature
dw 0xAA55                       ; Boot signature (0x7DFE, 0x7DFF)