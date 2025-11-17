; boot.asm - Stage 1 (Minimal CHS Read)
; Job: Load kernel.bin (Stage 2) into memory at 0x1000 and execute it.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

KERNEL_LOAD_SEGMENT     equ 0x0100  ; Load kernel to physical address 0x1000
KERNEL_LOAD_OFFSET      equ 0x0000
KERNEL_SECTORS          equ 40      ; Read 40 sectors (20KB)

start:
    cli                     ; Disable interrupts
    
    ; --- 1. Setup Segments and Stack (Use DS=0 for simplicity, assuming BIOS supports) ---
    xor ax, ax              
    mov ds, ax              ; DS = 0 (Access local data via absolute offset 0x7C00)
    mov es, ax              ; ES = 0 (Will be reset for load buffer)
    mov ss, ax              ; SS = 0
    mov sp, 0x7C00          ; Stack grows down from 0x7C00
    
    ; Save boot drive ID (DL is set by BIOS upon boot)
    ; Since DS=0, this stores DL at physical address 0x7DFD (0x7C00 + 0x1FD offset)
    mov [boot_drive_storage], dl
    
    sti                     ; Re-enable interrupts
    
    ; --- 2. Reset the disk controller (with retries) ---
    mov cx, 3  ; Number of retries
.reset_loop:
    push cx
    mov ah, 0x00            ; Function 0x00: Reset Disk System
    mov dl, [boot_drive_storage] ; Use the saved drive number
    int 0x13
    jnc .load_stage2        ; If carry flag is clear, success! Jump to load.
    pop cx
    loop .reset_loop        ; If carry flag was set (error), try again.
    
    jmp disk_error          ; If all retries fail, then show error.

.load_stage2:
    ; Restore CX if loop succeeded (it was popped inside the loop in the original code, 
    ; but restoring here ensures stack sanity if reset failed on first try)
    pop cx ; Discard the previous CX pushed before the .reset_loop

    ; --- 3. Load the kernel from disk (with retries) ---
    mov cx, 3  ; Reset retry counter for the read operation
.read_loop:
    push cx
    
    mov ax, KERNEL_LOAD_SEGMENT
    mov es, ax              ; ES = 0x0100 (Destination segment: 0x1000)
    mov bx, KERNEL_LOAD_OFFSET ; BX = 0x0000 (Destination offset)

    mov ah, 0x02            ; Function 0x02: Read Sectors
    mov al, KERNEL_SECTORS  ; AL = 40 (Read 40 sectors)
    mov ch, 0               ; CH = Cylinder 0
    mov cl, 2               ; CL = Sector 2 (Sector 1 is the boot sector, start reading at 2)
    mov dh, 0               ; DH = Head 0
    mov dl, [boot_drive_storage] ; Drive number (restored from memory)
    
    int 0x13
    jnc .jump_to_stage2     ; If carry flag is clear, success!
    
    ; If read failed, reset the disk system again before retrying
    push ax                 ; Save AX
    mov ah, 0x00            ; Function 0x00: Reset Disk System
    mov dl, [boot_drive_storage]
    int 0x13
    pop ax                  ; Restore AX
    
    pop cx
    loop .read_loop         ; Try reading again
    
    jmp disk_error          ; If all retries fail, show error.

.jump_to_stage2:
    ; --- 4. Prepare Segments for Stage 2 (Must be correct for Stage 2's ORG) ---
    mov ax, KERNEL_LOAD_SEGMENT
    mov ds, ax              ; DS = 0x0100
    mov es, ax              ; ES = 0x0100
    
    ; --- 5. Jump to the loaded kernel ---
    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET ; Far jump to 0x0100:0x0000

; Error handling
disk_error:
    mov si, msg_disk_error
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
msg_disk_error: db 'Disk read error!', 0

; Padding to 512 bytes total. boot_drive_storage needs to be at 0x1FD offset.
times 510 - ($ - $$) - 1 db 0   
boot_drive_storage: db 0        ; Store drive ID here (at 0x7DFD physical, 0x1FD offset)
dw 0xAA55                       ; Boot signature (0x7DFE, 0x7DFF)