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

.read_loop:
    ; Check if we're done
    cmp word [sectors_to_read], 0
    je .a20_stage                   ; All sectors read, jump to A20

    ; --- Convert LBA to CHS ---
    ; LBA is in [current_lba]
    ; Formulas (for 1.44MB floppy):
    ; SectorsPerTrack = 18
    ; HeadsPerCylinder = 2
    ; SectorsPerCylinder = 36
    ;
    ; Cylinder = LBA / 36
    ; Temp = LBA % 36
    ; Head = Temp / 18
    ; Sector = (Temp % 18) + 1
    
    mov ax, word [current_lba+2]    ; High word of LBA
    mov dx, word [current_lba]      ; Low word of LBA
    
    xor di, di                      ; di = 0
    mov cx, 36                      ; cx = SectorsPerCylinder
    div cx                          ; dx:ax / 36 -> ax = Cylinder, dx = Temp
    
    push ax                         ; Save Cylinder
    
    ; Now ax = Cylinder, dx = Temp
    xor ax, ax                      ; Clear ax
    mov ax, dx                      ; ax = Temp
    xor dx, dx                      ; Clear dx
    mov di, 18                      ; di = SectorsPerTrack
    div di                          ; dx:ax / 18 -> ax = Head, dx = Sector-1
    
    mov byte [head_temp], ah        ; Save Head (it's in AH)
    
    inc dx                          ; dx = Sector (1-based)
    
    ; Now we have:
    ; [stack] = Cylinder
    ; AL = Head (low 8 bits)
    ; DX = Sector
    
    mov cl, dl                      ; Sector
    pop ax                          ; Retrieve Cylinder
    mov ch, al                      ; Cylinder low 8 bits
    mov dh, al
    shr dh, 2                       ; Get high 2 bits of Cylinder
    and dh, 0xC0                    ; Isolate
    or cl, dh                       ; CL = Sector (bits 0-5) | Cyl high (bits 6-7)
    
    mov dh, byte [head_temp]        ; Retrieve Head
    ; DL = drive (already set from start)

    ; --- Determine sectors to read ---
    ; We read up to the end of the current track to be safe.
    ; AL = SectorsPerTrack(18) - Sector(CL bits 0-5) + 1
    mov al, cl
    and al, 0x3F                    ; Isolate sector number (bits 0-5)
    mov ah, 18
    sub ah, al
    inc ah                          ; AH = sectors left on track
    mov al, ah                      ; AL = sectors left on track
    
    ; --- FIX: Compare 16-bit values ---
    xor ah, ah                      ; Zero-extend AL (sectors this track) into AX
    
    mov cx, [sectors_to_read]       ; Get total remaining sectors (16-bit)
    cmp ax, cx                      ; Compare sectors_this_track (AX) to total (CX)
    jbe .read_count_ok              ; If ax <= cx, read ax sectors
    mov ax, cx                      ; Otherwise, read remaining sectors (cx)
.read_count_ok:
    mov byte [sectors_this_read], al ; Save for later
    ; --- END FIX ---

    ; --- Attempt Read (with retries) ---
    mov cx, 3                       ; Retry count
.retry_read:
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
    
    ; Read failed, reset disk and retry
    pusha
    mov ah, 0x00
    ; DL = drive (still set)
    int 0x13
    popa
    
    dec cx
    jnz .retry_read
    
    jmp disk_error                  ; All retries failed

.read_success:
    ; --- Update counters and pointers ---
    mov al, [sectors_this_read]
    xor ah, ah                      ; AX = sectors we just read
    
    ; Update sectors_to_read
    sub [sectors_to_read], ax       ; Decrease remaining count

    ; Update current_lba
    add [current_lba], ax
    adc word [current_lba+2], 0
    
    ; Update destination ES:BX
    ; We add (sectors_read * 512) to ES:BX
    ; 512 / 16 = 32. So we add (sectors_read * 32) to ES.
    mov cx, 32
    mul cx                          ; AX = sectors_read * 32
    add es, ax                      ; Add to segment
    
    jmp .read_loop                  ; Read next chunk

.a20_stage:
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

    ; Load the Global Descriptor Table
    lgdt [gdt_descriptor]

    ; Switch to protected mode by setting the PE bit in CR0
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

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
    ; We loaded it at 0x10000, and our GDT has a base of 0,
    ; so we can just jump directly to that address.
    jmp KERNEL32_JUMP_ADDRESS

    ; We should never get here
    cli
    hlt

; --- Padding ---
; Pad the rest of the file to 20KB (40 sectors * 512 bytes)
times 20480 - ($ - $$) db 0