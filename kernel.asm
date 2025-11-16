; kernel.asm
;
; Stage 2 bootloader.
; Loaded by stage 1 at 0x1000.
; Responsible for switching to 32-bit protected mode and jumping to the kernel.
;

[BITS 16]
[ORG 0x1000] ; We are now loaded at physical address 0x1000

KERNEL32_LOAD_SEGMENT equ 0x1000  ; 16-bit segment (0x1000 * 16 = 0x10000)
KERNEL32_JUMP_ADDRESS equ 0x10000 ; 32-bit physical address
BOOT_DRIVE_ADDRESS equ 0x7DFD ; The physical address where boot.asm stored the drive ID

start:
    cli                         ; Disable interrupts
    mov ax, cs                  ; Set DS to our own code segment (0x1000)
    mov ds, ax                  ; so we can access our data

    ; Load drive ID. We can access BOOT_DRIVE_ADDRESS directly
    ; because it's in a different segment (0x0000).
    push ds
    mov ax, 0x0000              ; Set DS to 0 to access low memory
    mov ds, ax
    mov dl, [BOOT_DRIVE_ADDRESS] ; Load drive ID saved by stage 1
    pop ds                      ; Restore DS back to 0x1000
    
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
    mov ah, 0x0E
    mov al, '1'
    int 0x10

; --- 2. Load the kernel from disk (with retries) ---
    ; We must use CHS (ah=0x02) because LBA (ah=0x42) is not supported.
    ; We need to read 256 sectors starting from LBA 41
    ; to ES:BX = 0x1000:0x0000 (physical 0x10000)
    
    mov word [sectors_to_read], 256
    mov dword [current_lba], 41
    
    mov ax, KERNEL32_LOAD_SEGMENT   ; ES = 0x1000
    mov es, ax
    mov bx, 0x0000                  ; BX = 0x0000
                                    ; ES:BX = 0x1000:0x0000 (physical 0x10000)

    ; --- CHECKPOINT 2 ---
    mov ah, 0x0E
    mov al, '2'
    int 0x10

.read_loop:
    ; --- CHECKPOINT 'a' (start of loop) ---
    mov ah, 0x0E
    mov al, 'a'
    int 0x10

    ; Check if we're done
    cmp word [sectors_to_read], 0
    je .a20_stage                   ; All sectors read, jump to A20

    ; --- Convert LBA to CHS ---
    mov dx, word [current_lba+2]    ; High word of LBA into DX
    mov ax, word [current_lba]      ; Low word of LBA into AX
    
    xor di, di                      
    mov cx, 36                      
    div cx                          ; 32-bit DX:AX / 16-bit CX
                                    ; AX = Cylinder, DX = Remainder (Temp)
    
    push ax                         ; Save Cylinder (in AX)
    
    mov ax, dx                      ; Remainder (Temp) into AX
    xor dx, dx                      ; Zero DX for 16-bit division
    mov di, 18                      
    div di                          ; 16-bit AX / 16-bit DI
                                    ; AX = Head, DX = Sector-1

    ; --- FIX 1: Use 16-bit remainder from DX ---
    mov byte [head_temp], al        ; AL is Head (from AX)
    
    inc dx                          ; Increment 16-bit remainder (Sector-1)
    mov cl, dl                      ; CL = Sector (1-based)
    ; --- END FIX 1 ---
    
    pop ax                          ; AX = Cylinder (10 bits, C9 C8 ... C0)
    mov ch, al                      ; CH = Cylinder low 8 bits (C7...C0)
    
    ; --- FIX 2: Correctly mask high bits ---
    mov dl, ah                      ; DL = Cylinder high 8 bits (000000 C9 C8)
    and dl, 0x03                    ; Mask to get only C9, C8
    shl dl, 6                       ; Shift to bits 7 and 6
    or cl, dl                       ; Combine with Sector
    ; --- END FIX 2 ---
    
    mov dh, byte [head_temp]        ; Retrieve Head
    ; DL = drive (still set from start)


    ; --- CHECKPOINT 'b' (LBA->CHS done) ---
    mov ah, 0x0E
    mov al, 'b'
    int 0x10

    ; --- Determine sectors to read ---
    mov al, cl
    and al, 0x3F                    
    mov ah, 18
    sub ah, al
    inc ah                          
    mov al, ah                      
    xor ah, ah                      
    mov cx, [sectors_to_read]       
    cmp ax, cx                      
    jbe .read_count_ok              
    mov ax, cx                      
.read_count_ok:
    mov byte [sectors_this_read], al 

    ; --- Attempt Read (with retries) ---
    mov cx, 3                       ; Retry count
.retry_read:
    ; --- CHECKPOINT 'c' (Before read) ---
    mov ah, 0x0E
    mov al, 'c'
    int 0x10

    pusha
    mov ah, 0x02                    ; Function: Read Sectors
    mov al, [sectors_this_read]     ; AL = sectors to read
    ; CH = Cylinder 
    ; CL = Sector / Cyl high
    ; DH = Head
    ; DL = Drive (set at start)
    ; ES:BX = Destination (set at start / updated in loop)
    int 0x13
    popa
    jnc .read_success               ; Success!
    
    ; --- CHECKPOINT 'e' (Read failed, before reset) ---
    mov ah, 0x0E
    mov al, 'e'
    int 0x10

    ; Read failed, reset disk and retry
    pusha
    mov ah, 0x00
    ; DL = drive (still set)
    int 0x13
    popa
    
    ; --- CHECKPOINT 'f' (After reset) ---
    mov ah, 0x0E
    mov al, 'f'
    int 0x10

    dec cx
    jnz .retry_read
    
    jmp disk_error                  ; All retries failed

.read_success:
    ; --- CHECKPOINT 'd' (Read success) ---
    mov ah, 0x0E
    mov al, 'd'
    int 0x10

    ; --- Update counters and pointers ---
    mov al, [sectors_this_read]
    xor ah, ah                      ; AX = sectors we just read
    
    sub [sectors_to_read], ax       ; Decrease remaining count
    add [current_lba], ax
    adc word [current_lba+2], 0
    
    mov cx, 32
    mul cx                          ; AX = sectors_read * 32 (paragraph offset)
    
    push bx                         
    mov bx, es                      
    add bx, ax                      
    mov es, bx                      
    pop bx                          
    
    ; --- CHECKPOINT 'g' (Loop done, about to jmp) ---
    mov ah, 0x0E
    mov al, 'g'
    int 0x10

    jmp .read_loop                  ; Read next chunk

.a20_stage:
    ; --- CHECKPOINT 3 ---
    mov ah, 0x0E
    mov al, '3'
    int 0x10

; Enable A20 line (via keyboard controller)
    call wait_for_input         ; Wait for input buffer to be empty
    mov al, 0xAD                ; Command: Disable keyboard
    out 0x64, al
    
    call wait_for_input
    mov al, 0xD0                ; Command: Read from controller output port
    out 0x64, al
    
    call wait_for_output        ; Wait for output buffer to be full
    in al, 0x60                 ; Read output port
    push eax                    ; Save current state
    
    call wait_for_input
    mov al, 0xD1                ; Command: Write to controller output port
    out 0x64, al
    
    call wait_for_input
    pop eax                     ; Get old state
    or al, 0x02                 ; Set bit 1 (A20 Gate)
    out 0x60, al                ; Write new state back
    
    call wait_for_input
    mov al, 0xAE                ; Command: Enable keyboard
    out 0x64, al
    
    call wait_for_input

    ; --- CHECKPOINT 4 ---
    mov ah, 0x0E
    mov al, '4'
    int 0x10

    ; Load the Global Descriptor Table
    lgdt [gdt_descriptor]

    ; --- CHECKPOINT 5 ---
    mov ah, 0x0E
    mov al, '5'
    int 0x10

    ; Switch to protected mode by setting the PE bit in CR0
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    ; --- CHECKPOINT 6 ---
    ; This will print *right before* we jump to 32-bit mode
    mov ah, 0x0E
    mov al, '6'
    int 0x10

    ; Far jump to our 32-bit code segment (selector 0x08).
    ; This jump also clears the CPU's instruction pipeline.
    ; We use a 32-bit operand size prefix (0x66)
    db 0x66
    jmp 0x08:protected_mode_start

; 16-bit helper routines for A20
wait_for_input:
    in al, 0x64
    test al, 0x02
    jnz wait_for_input
    ret

wait_for_output:
    in al, 0x64
    test al, 0x01
    jz wait_for_output
    ret

; --- Data for read loop ---
sectors_to_read:    dw 0
sectors_this_read:  db 0
head_temp:          db 0
current_lba:        dd 0

disk_error:
    ; --- CHECKPOINT 'E' (ERROR) ---
    mov ah, 0x0E
    mov al, 'E'
    int 0x10

    ; Simple error handling: just halt.
    ; We are in a weird state (halfway to protected mode), so don't try video.
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
    dd gdt_start                ; Base address of GDT

; ==================================================================
; We are now in 32-bit Protected Mode!
; =================================_
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
    ; We loaded it at 0x10000, and our GDT has a base of 0,
    ; so we can just jump directly to that address.
    jmp KERNEL32_JUMP_ADDRESS

    ; We should never get here
    cli
    hlt

; --- Padding ---
; Pad the rest of the file to 20KB (40 sectors * 512 bytes)
times 20480 - ($ - $$) db 0