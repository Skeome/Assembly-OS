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
current_lba:        dd 0


print_char:                 ; Routine to print a character in AL
    mov ah, 0x0E
    int 0x10
    ret

start:
    cli                         ; Disable interrupts
    
    xor dx, dx                  ; Clear DX, including DL (drive)

    ; --- Set up the stack and segment registers ---
    ; We are loaded at 0x1000. We must set up our own stack
    ; before we can call any interrupts (like int 0x13).
    ; We will set the stack to grow down from our load address.
    
    xor ax, ax                  ; AX = 0
    mov ds, ax                  ; DS = 0 (for accessing data by physical address)
    mov es, ax                  ; ES = 0 (for... just in case)
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
    ; DL (drive) was loaded up top
    int 0x13
    jnc .read_kernel_stage  ; If carry flag is clear, success!
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
    ; --- Convert LBA to CHS ---
    
    ; --- FIX: Be explicit about 32-bit divide ---
    xor edx, edx                    ; Clear high 32-bits of EDX:EAX
    mov dx, word [current_lba+2]    ; dx = high word of LBA
    mov ax, word [current_lba]      ; ax = low word of LBA
    
    mov di, 36                      ; 18 sectors/track * 2 heads
    div di                          ; 32-bit EDX:EAX / 16-bit DI
                                    ; AX = Cylinder, DX = Remainder (Temp)
    
    push ax                         ; Save Cylinder (in AX)
    
    mov ax, dx                      ; Remainder (Temp) into AX
    xor dx, dx                      ; Zero DX for 16-bit division
    mov di, 18                      ; 18 sectors/track
    div di                          ; 16-bit AX / 16-bit DI
                                    ; Quotient -> AX (Head)
                                    ; Remainder -> DX (Sector-1)

    ; --- This was the bug. Results are in AX (Head) and DX (Sector-1) ---
    mov byte [head_temp], al        ; AL is Head (from AX)
    inc dx                          ; Increment 16-bit remainder (Sector-1)
    mov cl, dl                      ; CL = Sector (1-based)
    
    pop ax                          ; AX = Cylinder (10 bits, C9 C8 ... C0)
    mov ch, al                      ; CH = C'ylinder low 8 bits (C7...C0)
    
    mov dl, ah                      ; DL = Cylinder high 2 bits (000000 C9 C8)
    and dl, 0x03                    ; Mask to get only C9, C8
    shl dl, 6                       ; Shift to bits 7 and 6
    or cl, dl                       ; Combine with Sector
    
    mov dh, byte [head_temp]        ; Retrieve Head
    
    ; --- Determine sectors to read ---
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
    pusha                           ; Saves all registers (ES, BX, CX, DX)
    mov ah, 0x02                    ; Function: Read Sectors
    mov al, [sectors_this_read]     ; AL = sectors to read
    
    ; DL (drive) needs to be reloaded (it was clobbered by CHS math)
    mov dl, [BOOT_DRIVE_ADDRESS]
    
    ; ES:BX, CX, DX were set before pusha
    
    int 0x13
    popa                            ; Restores all registers
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
    jmp disk_error                  

.read_success:
    ; --- Update counters and pointers ---
    mov al, [sectors_this_read]
    xor ah, ah                      ; AX = sectors we just read
    
    sub [sectors_to_read], ax       ; Decrease remaining count
    add [current_lba], ax
    adc word [current_lba+2], 0
    
    ; Calculate new paragraph offset for ES
    mov cx, 32
    mul cx                          ; AX = paragraph offset to add (DX is clobbered)
    
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

    ; --- Use reliable BIOS call to enable A20 ---
    mov ah, 0x0C                ; Function 0Ch: Set A20 Gate Status
    mov al, 0x01                ; Enable A20 Gate
    int 0x15                    ; Call BIOS
    jc disk_error               ; Jump if error (e.g., function not supported)

    ; --- CHECKPOINT 4 ---
    mov al, '4'
    call print_char

    ; --- Load GDT ---
    lgdt [gdt_descriptor]

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

    ; Far jump
    db 0x66
    jmp 0x08:protected_mode_start

    ; --- CHECKPOINT 7 ---
    lidt [0]
    hlt

disk_error:
    ; --- CHECKPOINT 'E' (ERROR) ---
    mov ah, 0x0E
    mov al, 'E'
    int 0x10

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
protected_mode_start:

    ; --- 1. Set up 32-bit segment registers ---
    ; CS is already 0x08 from our far jump.
    ; Now we set all data segments to 0x10.
    mov ax, 0x10    ; 0x10 is our Data Segment selector
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax      ; Set up the stack segment

    ; --- 2. Set up the stack ---
    ; Set the stack pointer to a high memory address
    mov esp, 0x90000

    ; --- 3. Jump to the 32-bit kernel entry point ---
    jmp KERNEL32_JUMP_ADDRESS

    ; We should never get here
    cli
    hlt

; --- Padding ---
; Pad the rest of a 512-byte sector
times 20480 - ($ - $$) db 0