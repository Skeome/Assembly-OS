; kernel.asm
;
; Stage 2 bootloader.
; Loaded by stage 1 at 0x1000.
; Responsible for switching to 32-bit protected mode and jumping to the kernel.
;

[BITS 16]
[ORG 0x1000] ; We are now loaded at physical address 0x1000

KERNEL_ENTRY_POINT equ 0x10000 ; Address where we will load the 32-bit kernel
BOOT_DRIVE_ADDRESS equ 0x7DFD ; The physical address where boot.asm stored the drive ID

start:
    cli                         ; Disable interrupts
    
    ; Load the 32-bit kernel (kernel32.bin) from disk
    mov ax, KERNEL_ENTRY_POINT
    mov es, ax                  ; Destination segment (0x1000)
    mov bx, 0x0000              ; Destination offset (0x0000) -> physical 0x10000
    
    mov ah, 0x02                ; Function: "Read Sectors"
    mov al, 255                 ; Read max sectors (127.5 KB)
    mov ch, 0                   ; Track 0
    mov cl, 42                  ; Start at sector 42 (1-based)
    mov dh, 0                   ; Head 0
    
    ; Restore boot drive number
    push ds
    mov ax, 0x0000              ; Set DS to 0 to access low memory
    mov ds, ax
    mov dl, [BOOT_DRIVE_ADDRESS] ; Load drive ID saved by stage 1
    pop ds
    
    ; Reset disk controller (just in case)
    push ax                     ; Save AX (read params)
    mov ah, 0x00
    int 0x13
    jc disk_error               ; Handle disk error
    pop ax                      ; Restore AX

    ; Read the kernel
    mov dl, [BOOT_DRIVE_ADDRESS] ; Reload drive number (int 0x13/ah=0x00 corrupts it)
    int 0x13
    jc disk_error               ; Handle disk error

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
    jmp KERNEL_ENTRY_POINT

    ; We should never get here
    cli
    hlt

; --- Padding ---
; Pad the rest of the file to 20KB (40 sectors * 512 bytes)
times 20480 - ($ - $$) db 0