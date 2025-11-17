; kernel.asm
;
; Stage 2 bootloader.
; Loaded by stage 1 at 0x1000.
; Responsible for switching to 32-bit protected mode and jumping to the kernel.
;

[BITS 16]
[ORG 0x1000] ; We are loaded at physical address 0x1000

KERNEL32_LOAD_SEGMENT equ 0x1000  ; 16-bit segment (0x1000 * 16 = 0x10000)
KERNEL32_JUMP_ADDRESS equ 0x10000 ; 32-bit physical address
BOOT_DRIVE_ADDRESS equ 0x7DFD ; The physical address where boot.asm stored the drive ID

; --- Data for read loop ---
sectors_to_read:    dw 0
sectors_this_read:  db 0
head_temp:          db 0
current_lba:        dd 0          ; Still 32-bit for the count
disk_error_code:    db 0          ; Stores AH from int 0x13 failure

print_char:                 ; Routine to print a character in AL
    mov ah, 0x0E
    int 0x10
    ret

; Utility to print AH as a single hex digit (0-F) for debugging
print_hex_digit:
    push ax
    push bx
    push cx
    
    ; Get last 4 bits of AL (which holds AH from error path)
    and al, 0x0F
    cmp al, 0x09
    jbe .is_digit
    add al, 0x07   ; A-F needs an offset of 7 from 9 (A is 10, 'A' is 65. 10 + 7 = 17, 17+48 = 65)
.is_digit:
    add al, 0x30   ; Add '0' ASCII offset
    call print_char
    
    pop cx
    pop bx
    pop ax
    ret
    
; --- String Printing Utility ---
; Expects string address in SI (DS:SI)
print_string:
    pusha
.loop_str:
    lodsb                   ; Load char from [SI] into AL and increment SI
    cmp al, 0               ; Check for null terminator
    je .done_str
    call print_char
    jmp .loop_str
.done_str:
    popa
    ret
    
start:
    cli                         ; Disable interrupts
    
    ; --- Set up the stack and segment registers ---
    xor ax, ax                  ; AX = 0
    mov ds, ax                  ; DS = 0 (for accessing data by physical address)
    mov es, ax                  ; ES = 0
    mov ss, ax                  ; SS = 0
    mov sp, 0x9000              ; Set stack pointer to a safe place (9000h)
    
    sti                         ; Re-enable interrupts

    ; Load drive ID. DS is already 0, so we can access 0x7DFD directly.
    mov dl, [BOOT_DRIVE_ADDRESS] ; Load drive ID saved by stage 1
    
    ; --- 1. Reset the disk controller (with retries) ---
    ; NOTE: We are removing the loop and error jump temporarily for debugging stability.
    
    mov ah, 0x00            ; Function 0x00: Reset Disk System
    int 0x13
    
    ; --- CHECKPOINT 1 (Primary Checkpoint) ---
    mov al, '1'
    call print_char

; --- 2. Load the kernel from disk using CHS (reverted) ---
    
    mov word [sectors_to_read], 256
    mov dword [current_lba], 41
    
    mov ax, KERNEL32_LOAD_SEGMENT   ; ES = 0x1000
    mov es, ax
    mov bx, 0x0000                  ; BX = 0x0000 (Load to 0x1000:0000 = 0x10000)

    ; --- CHECKPOINT 2 ---
    mov al, '2'
    call print_char

.read_loop:
    ; LBA is currently in [current_lba]
    
    mov ax, word [current_lba]      ; AX = LBA (low 16 bits)
    xor dx, dx                      ; DX:AX = LBA
    
    ; Calculate Head (H): LBA % (SectorsPerTrack * Heads) / SectorsPerTrack
    mov cx, 18                      ; Divisor (SectorsPerTrack)
    div cx                          ; AX = C/H Temp, DX = LBA % 18 (Sector-1 Remainder)
    
    inc dl                          ; DL = Sector (1-based)
    mov cl, dl                      ; CL = Sector (1-based, S0...S5)
    
    mov al, dh                      ; AL = C/H Temp (Original quotient)
    mov dh, 0                       ; DH = Head (H) (Final H is 0 or 1 for floppies)

    ; Calculate Cylinder (C): LBA / SectorsPerCylinder (36)
    mov ax, word [current_lba]
    xor dx, dx
    mov cx, 36
    div cx                          ; AX = C, DX = Remainder (unused)

    ; Combine 10-bit Cylinder (C) and Sector (S) into CL/CH pair (BIOS format)
    mov ch, al                      ; CH = Cylinder low 8 bits (C7...C0)
    
    ; High 2 bits of Cylinder (C8, C9) go into bits 7, 6 of CL
    mov al, ah                      ; AL = C High 2 bits (from AH)
    and al, 0x03                    ; Mask to get only C8, C9
    shl al, 6                       ; Shift C8, C9 to CL bits 7, 6
    or cl, al                       ; Combine with Sector
    
    ; DH (Head) is already 0. DL (Drive) will be loaded below.
    
    ; --- Determine sectors to read (relying on CL) ---
    mov al, cl
    and al, 0x3F                    ; al = sector (1-18)
    mov ah, 18
    sub ah, al                      ; ah = 18 - sector
    inc ah                          ; ah = sectors left on track
    mov al, ah                      
    xor ah, ah                      ; ax = sectors left on track
    
    mov di, [sectors_to_read]       ; DI = total sectors remaining
    cmp ax, di                      
    jbe .read_count_ok              
    mov ax, di                      ; ax = min(sectors_on_track, sectors_remaining)
.read_count_ok:
    mov byte [sectors_this_read], al 

    ; --- Attempt Read (with retries) ---
    mov di, 3                       ; Retry count in DI
.retry_read:
    pusha                           ; Saves all registers
    mov ah, 0x02                    ; Function: Read Sectors
    mov al, [sectors_this_read]     ; AL = sectors to read
    
    ; DL (drive) needs to be reloaded 
    mov dl, [BOOT_DRIVE_ADDRESS]
    
    int 0x13
    
    ; Check AH for the status code after the interrupt
    mov byte [disk_error_code], ah
    
    popa                            ; Restores all registers (BX, CX, DX are restored with CHS params)
    jnc .read_success               ; Success!
    
    ; Read failed, reset disk and retry
    pusha
    mov dl, [BOOT_DRIVE_ADDRESS]
    mov ah, 0x00
    int 0x13
    popa
    
    dec di                          ; Decrement retry counter in DI
    jnz .retry_read
    
    ; --- Read failed after all retries ---
    mov al, 'F'
    call print_char
    
    mov al, [disk_error_code]
    call print_hex_digit
    
    jmp disk_error                  

.read_success:
    ; --- Update counters and pointers ---
    mov al, [sectors_this_read]
    xor ah, ah                      ; AX = sectors we just read
    
    sub [sectors_to_read], ax       ; Decrease remaining count

    add word [current_lba], ax      ; Add AX (sectors read) to current_lba (low 16)
    adc word [current_lba+2], 0     ; Add carry to current_lba (high 16)
    
    ; Calculate new paragraph offset for ES
    mov cx, 32
    mul cx                          ; AX = paragraph offset to add
    
    push bx                         ; Save BX (which is 0)
    mov bx, es                      
    add bx, ax                      
    mov es, bx                      
    pop bx                          ; Restore B (back to 0)
    
    cmp word [sectors_to_read], 0
    jz .a20_stage                   ; All sectors read, jump to A20

    jmp .read_loop                  ; Read next chunk

.a20_stage:
    ; --- CHECKPOINT 3 ---
    mov al, '3'
    call print_char

    ; --- Enable A20 Gate ---
    mov ax, 0x2402              ; Function 2402h: Enable A20 Gate
    int 0x15                    ; Call BIOS
    jc disk_error               ; Jump if error 

    ; --- CHECKPOINT 4 ---
    mov al, '4'
    call print_char

    ; --- Load GDT ---
    lgdt [gdt_descriptor]

    mov ax, 0x10  ; The Data Segment Selector
    mov ss, ax    ; Set the Stack Segment to a valid selector

    ; --- CHECKPOINT 5 ---
    mov al, '5'
    call print_char

    ; --- CHECKPOINT 6 ---
    mov al, '6'
    call print_char

    ; --- WAIT FOR KEYPRESS (16-bit) ---
    mov si, wait_msg_16bit
    call print_string
    
    mov ah, 0x00                ; BIOS function 0x00: Wait for keypress
    int 0x16                    ; Waits here until a key is pressed.

    ; --- Setup 32-bit Stack Pointer (CRITICAL FIX) ---
    ; Set 32-bit stack pointer (ESP) before enabling Protected Mode
    mov esp, 0x90000            ; Set ESP to the high address used by kernel32.asm

    ; Switch to protected mode
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    
    push dword KERNEL32_JUMP_ADDRESS ; Pushes 0x10000 (EIP)
    push dword 0x08                  ; Pushes 0x08 (CS Selector)
    retf                             ; Atomically loads EIP=0x10000, CS=0x08, and starts execution at kernel32.asm

disk_error_big_lba:
    mov si, msg_big_lba
    jmp print_string_and_halt

disk_error:
    ; --- CHECKPOINT 'E' (ERROR) ---
    mov ah, 0x0E
    mov al, 'E'
    int 0x10
    
    mov si, msg_disk_error_generic ; Print generic error on failure
    jmp print_string_and_halt

print_string_and_halt:
    ; Print routine for errors (expects string in SI)
    push ax
    push bx
    push cx
    push si ; Save original SI, as print_string uses it
    
.loop_str_halt:
    lodsb
    cmp al, 0
    je .halt_str
    call print_char
    jmp .loop_str_halt
    
.halt_str:
    pop si ; Restore SI before continuing to popa
    pop cx
    pop bx
    pop ax
    cli
    hlt
    jmp $

; --- Global Descriptor Table (GDT) ---
gdt_start:
    ; Null descriptor (required)
    dq 0x0

    ; Code segment descriptor (selector 0x08)
    dw 0xFFFF                   ; Limit (low 16 bits)
    dw 0x0000                   ; Base (low 16 bits)
    db 0x00                     ; Base (mid 8 bits)
    db 0b10011010               ; Access (Present, Ring 0, Code, Executable, Readable)
    db 0b11001111               ; Flags (Granularity=4K, 32-bit) + Limit (high 4 bits)
    db 0x00                     ; Base (high 8 bits)

    ; Data segment descriptor (selector 0x10)
    dw 0xFFFF                   ; Limit (low 16 bits)
    dw 0x0000                   ; Base (low 16 bits)
    db 0x00                     ; Base (mid 8 bits)
    db 0b10010010               ; Access (Present, Ring 0, Data, Writable)
    db 0b11001111               ; Flags (Granularity=4K, 32-bit) + Limit (high 4 bits)
    db 0x00                     ; Base (high 8 bits)

gdt_end:

; GDT descriptor (pointer) for the LGDT instruction
gdt_descriptor:
    dw gdt_end - gdt_start - 1  ; Limit (size of GDT - 1)
    dd gdt_start                ; Base address of GDT (physical address due to ORG and DS=0)

; ==================================================================
; We are now in 32-bit Protected Mode!
; ==================================================================
[BITS 32]
; This section is now EMPTY. Execution jumps directly to KERNEL32_JUMP_ADDRESS (0x10000).

msg_big_lba: db 'LBA too large for 16-bit read!', 0
msg_disk_error_generic: db 'Disk I/O error during stage 2 load!', 0
wait_msg_16bit: db 'Boot stage 2 complete (16-bit mode). Press any key to continue to 32-bit kernel...', 0

; --- Padding ---
; Pad the rest of a 512-byte sector
times 20480 - ($ - $$) db 0