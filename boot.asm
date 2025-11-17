; boot.asm
;
; This is the boot sector, loaded by the BIOS at 0x7C00.
; Job: Load the kernel (Stage 2) using reliable 1-sector CHS read loops.
;-----------------------------------------------------------------------------

[ORG 0x7C00]
[BITS 16]

KERNEL_LOAD_SEGMENT     equ 0x0100  ; Load to 0x1000
KERNEL_LOAD_OFFSET      equ 0x0000

start:
    cli                     ; Disable interrupts
    
    ; Set up segments (DS=ES=SS=0) and stack
    xor ax, ax              
    mov ds, ax              
    mov es, ax              
    mov ss, ax              
    mov sp, 0x7C00          
    
    sti                     ; Re-enable interrupts
    
    ; Save boot drive ID (DL) to 0x7DFD
    mov [boot_drive_storage], dl

    ; --- 1. Reset the disk controller (with retries) ---
    mov cx, 3  ; Number of retries
.reset_loop:
    push cx
    mov ah, 0x00            ; Function 0x00: Reset Disk System
    mov dl, [boot_drive_storage]
    int 0x13
    jnc .init_load          ; Success, jump to load setup
    pop cx
    loop .reset_loop
    
    jmp disk_error_reset    ; If reset fails

.init_load:
    pop cx                  ; Discard the 'cx' value

    ; --- Checkpoint A (Reset success) ---
    mov al, 'A'
    call print_char
    
    ; Set initial load parameters
    mov ax, KERNEL_LOAD_SEGMENT
    mov es, ax              ; ES:BX = 0x1000 (Load address)
    mov bx, KERNEL_LOAD_OFFSET
    
    mov ch, 0               ; Start Cylinder 0
    mov dh, 0               ; Start Head 0
    mov cl, 2               ; Start Sector 2 (LBA 1)
    
    mov si, 40              ; SI = Total sectors left to read (40 sectors for kernel.asm)
    
.read_loop:
    ; --- Checkpoint B (Inside Sector Read Loop - prints for every successful sector read) ---
    mov al, 'B'
    call print_char
    
    mov cx, 3               ; Retry count per sector
.sector_retry:
    push cx                 ; Save retry count
    push ax                 ; Save AL/AH for call
    
    ; Reset disk system before every read attempt (Best practice for floppies)
    mov ah, 0x00
    mov dl, [boot_drive_storage]
    int 0x13
    pop ax                  ; Restore AX
    
    ; --- Read ONE Sector ---
    mov ah, 0x02            ; Function 0x02: Read Sector(s)
    mov al, 1               ; Read exactly 1 sector
    
    ; CH, DH, CL are correctly set up before the loop and incremented after
    mov dl, [boot_drive_storage] ; Drive number
    
    int 0x13
    jnc .read_success       ; Success, jump out of retry loop
    
    ; Read failed, reset and retry
    pop cx                  ; Restore retry count
    loop .sector_retry      ; Retry reading the same sector
    
    jmp disk_error_load     ; Failed to read sector after 3 retries

.read_success:
    ; --- Geometry Increment ---
    inc cl                  ; Increment Sector
    cmp cl, 19              ; Did we pass the last sector (18)?
    jle .update_buffer      ; No wrap, just go update buffer

    mov cl, 1               ; Reset Sector to 1
    inc dh                  ; Increment Head
    cmp dh, 2               ; Did we pass the last head (1)?
    jle .update_buffer      ; No wrap, just go update buffer

    mov dh, 0               ; Reset Head to 0
    inc ch                  ; Increment Cylinder
    
.update_buffer:
    ; Update Load Address (ES:BX += 512 bytes)
    add bx, 512             ; Increment offset by 512 bytes
    ; Carry flag handling for ES increment omitted (assumes 40 sectors keep us below 64KB boundary)

    dec si                  ; Decrement total sector count (40 -> 0)
    cmp si, 0
    jne .read_loop          ; If SI is not zero, read next sector

    ; --- 3. Jump to the loaded kernel ---
    ; --- Checkpoint C (Load complete) ---
    mov al, 'C'
    call print_char

    mov ax, KERNEL_LOAD_SEGMENT
    mov ds, ax              ; Set DS to kernel segment
    mov es, ax              ; Set ES to kernel segment

    jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

; --------------------------------------
; Utilities
; --------------------------------------
print_char:                 
    mov ah, 0x0E
    int 0x10
    ret
    
; --------------------------------------
; Error handling
; --------------------------------------
disk_error_reset:
    mov si, msg_disk_error_reset
    jmp print_string_halt

disk_error_load:
    mov si, msg_disk_error_load
    jmp print_string_halt
    
print_string_halt:
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
msg_disk_error_reset: db 'Disk reset error! Halting.', 0
msg_disk_error_load:  db 'Kernel read error! Halting.', 0

; We need 512 bytes total. 
times 510 - ($ - $$) - 1 db 0   ; Pad
boot_drive_storage: db 0        ; Store drive ID here (at 0x7DFD)
dw 0xAA55                       ; Boot signature