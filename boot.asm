; boot.asm
;
; This is the boot sector, loaded by the BIOS at 0x7C00.
; Its job is to load the second stage (kernel.bin) into memory and execute it.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

KERNEL_LOAD_SEGMENT     equ 0x0100  ; We will load our kernel to 0100:0000 = 0x1000
KERNEL_LOAD_OFFSET    equ 0x0000

start:
    cli                     ; Disable interrupts
    
    ; Set up segments
    xor ax, ax              ; AX = 0
    mov ds, ax              ; DS = 0
    mov es, ax              ; ES = 0
    mov ss, ax              ; SS = 0
    mov sp, 0x7C00          ; Stack grows down from 0x7C00
    
    sti                     ; Re-enable interrupts
    
    ; Save boot drive to a safe location within the boot sector itself
    mov [boot_drive_storage], dl

    ; --- 1. Reset the disk controller (with retries) ---
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
    pop cx ; Discard the 'cx' value from the stack (we succeeded)

    ; --- 2. Load the kernel from disk (with retries) ---
    mov cx, 3  ; Reset retry counter for the read operation
.read_loop:
    push cx
    mov ax, KERNEL_LOAD_SEGMENT
    mov es, ax              ; ES:BX = 0100:0000 (physical 0x1000)
    mov bx, KERNEL_LOAD_OFFSET

    mov ah, 0x02            ; Function 0x02: Read Sectors
    mov al, 296             ; Read 296 sectors (296 * 512 = 153.6KB)
    mov ch, 0               ; Cylinder 0
    mov cl, 2               ; Sector 2 (Sector 1 is us)
    mov dh, 0               ; Head 0
    mov dl, [boot_drive_storage] ; Drive number (restored from memory)
    
    int 0x13
    jnc .jump_to_stage2     ; If carry flag is clear, success!
    
    ; If read failed, reset the disk system again before retrying
    push ax                 ; Save ax before calling reset
    mov ah, 0x00
    mov dl, [boot_drive_storage]
    int 0x13
    pop ax                  ; Restore ax
    
    pop cx
    loop .read_loop         ; Try reading again
    
    jmp disk_error          ; If all retries fail, show error.

.jump_to_stage2:
    ; --- 3. Jump to the loaded kernel ---
    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

; Error handling
disk_error:
    mov si, msg_disk_error
print_string:
    mov ah, 0x0E            ; BIOS teletype output
.loop:
    lodsb                   ; Load character from [SI] into AL
    cmp al, 0
    je .halt
    int 0x10                ; Call BIOS video interrupt
    jmp .loop
.halt:
    cli
    hlt

; --- Data ---
msg_disk_error: db 'Disk read error!', 0

; We need 512 bytes total. The magic number is 2 bytes at the end (510 and 511).
; We will put our boot drive ID at byte 509 (offset 0x1FD from 0x0000).
; Since ORG is 0x7C00, this will be at physical address 0x7C00 + 509 = 0x7DFD
times 510 - ($ - $$) - 1 db 0   ; Pad with 0s, leaving 1 byte before the magic number
boot_drive_storage: db 0        ; Store drive ID here (at 0x7DFD)
dw 0xAA55                       ; Boot signature