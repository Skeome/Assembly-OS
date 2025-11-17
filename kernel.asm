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
    mov cx, 3               ; Number of retries
.reset_loop:
    push cx
    mov ah, 0x00            ; Function 0x00: Reset Disk System
    int 0x13
    jnc .read_kernel_stage  ; If carry flag is clear, success!
    
    ; Save error code and try again
    mov [disk_error_code], ah
    pop cx
    loop .reset_loop        ; If carry flag was set (error), try again.

    jmp disk_error          ; If all retries fail

.read_kernel_stage:
    pop cx                  ; Discard retry counter from stack

    ; --- CHECKPOINT 1 ---
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
    ; --- Convert LBA (32-bit) to CHS (16-bit safe) ---
    ; LBA is < 1440 sectors, so AX (16 bits) can hold the LBA value, avoiding EAX/EDX complexity.
    
    push dx                         ; Save DX since we will use it for calculations
    
    ; Load the 32-bit LBA into AX (low 16) and DX (high 16)
    mov dx, word [current_lba+2]
    mov ax, word [current_lba]
    
    ; Since LBA is small (max 1440), high word DX should be 0. 
    ; If it's non-zero, the disk image is too big for this 16-bit routine.
    cmp dx, 0
    jnz disk_error_big_lba          ; Jump if LBA > 65535 sectors
    
    ; Divide LBA by (SectorsPerTrack * Heads) = 36 to get Cylinder (C)
    xor dx, dx                      ; DX:AX is now 32-bit LBA (max 0x0000FFFF)
    mov cx, 36                      ; Divisor (Sectors/Cylinder)
    div cx                          ; AX = C, DX = Remainder
    
    mov cl, al                      ; CL = Cylinder Low 8 bits
    mov ch, ah                      ; CH = Cylinder High 2 bits (This is wrong, C is in AX)
    
    ; Re-do Cylinder calculation:
    ; The result of the 16-bit DIV is: AX (Quotient=C), DX (Remainder)
    mov bl, al                      ; BL = Cylinder (0-255)
    
    ; Divide Remainder by SectorsPerTrack (18) to get Head (H)
    mov ax, dx                      ; Remainder into AX
    xor dx, dx                      
    mov cx, 18                      ; Divisor (Sectors/Track)
    div cx                          ; AX = H, DX = Sector-1
    
    mov dh, al                      ; DH = Head (H)
    inc dl                          ; DL = Sector (S) (1-based)

    ; Combine 10-bit Cylinder (C) and Sector (S) into CL/CH pair (BIOS format)
    mov ch, bl                      ; CH = Cylinder low 8 bits (C7...C0)
    mov cl, dl                      ; CL = Sector (S) (1-based, S0...S5)
    
    ; High 2 bits of Cylinder (C8, C9) go into bits 7, 6 of CL
    mov al, bl                      ; AL = Cylinder value
    and al, 0x0300                  ; Clear all but C8 and C9 (in AH)
    shl al, 6                       ; Shift C8, C9 to CL bits 7, 6
    or cl, al                       ; Combine with Sector
    
    ; Final CHS values are now in CX and DX (DH/DL)
    
    pop dx                          ; Restore original DX (drive ID), though we pushed it 
                                    ; earlier, we need to restore it here. We saved it 
                                    ; right before the div, so let's pop it now.

    mov dh, byte [head_temp]        ; Retrieve Head (H) (We didn't actually use DH/DL for H/S)
    
    ; The correct CHS registers are: CL, CH, DH (already calculated above)
    ; Drive DL is already correct.

    ; --- Re-Calculate CHS for simplicity, relying purely on 16-bit ops ---
    mov ax, word [current_lba]
    mov dx, 0                       ; DX:AX = LBA
    mov cx, 36
    div cx                          ; AX = C, DX = Remainder
    
    mov ch, al                      ; CH = Cylinder low 8 bits (C7...C0)
    mov al, ah                      ; AH = Cylinder high 2 bits (000000 C9 C8)
    
    mov al, dl                      ; AL = Remainder
    mov dx, 0
    mov cx, 18
    div cx                          ; AX = H, DX = Sector-1
    
    mov dh, al                      ; DH = Head
    inc dl                          ; DL = Sector (1-based)
    
    ; Combine Cylinder High bits (in AL) with Sector (in DL) into CL
    mov al, ch                      ; AL = C
    and al, 0x03                    ; C8, C9
    shl al, 6
    or cl, dl                       ; Final CL (S|C8|C9)
    mov ch, ch                      ; CH = C Low

    ; Drive DL is restored from [BOOT_DRIVE_ADDRESS] in the loop below.

    ; --- Determine sectors to read (same as before) ---
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
    
    ; Print the error code AH (only low byte needed)
    mov al, [disk_error_code]
    call print_hex_digit
    
    jmp disk_error                  

.read_success:
    ; --- Update counters and pointers ---
    mov al, [sectors_this_read]
    xor ah, ah                      ; AX = sectors we just read
    
    sub [sectors_to_read], ax       ; Decrease remaining count
    
    ; --- FIX: Update LBA as 32-bit value (using 32-bit add) ---
    db 0x66                         ; 32-bit operand size prefix
    add dword [current_lba], eax    ; Add AX (sectors read) to current_lba (32-bit)
    
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

    ; --- Use reliable BIOS call 0x15, AX=0x2402 to enable A20 ---
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

    ; Switch to protected mode
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    ; --- CRITICAL FIX: Far Return directly to KERNEL32_JUMP_ADDRESS (0x10000) ---
    
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

print_string_and_halt:
    ; Simple error handling: just halt.
    cli
    hlt
    jmp disk_error

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

; --- Padding ---
; Pad the rest of a 512-byte sector
times 20480 - ($ - $$) db 0