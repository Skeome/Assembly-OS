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
    mov word [sectors_to_read], 256
    mov dword [current_lba], 41
    
    mov ax, KERNEL32_LOAD_SEGMENT   ; ES = 0x1000
    mov es, ax
    mov bx, 0x0000                  ; BX = 0x0000

.read_loop:
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

    mov byte [head_temp], al        ; AL is Head (from AX)
    inc dx                          ; Increment 16-bit remainder (Sector-1)
    mov cl, dl                      ; CL = Sector (1-based)
    
    pop ax                          ; AX = Cylinder (10 bits, C9 C8 ... C0)
    mov ch, al                      ; CH = Cylinder low 8 bits (C7...C0)
    
    mov dl, ah                      ; DL = Cylinder high 8 bits (000000 C9 C8)
    and dl, 0x03                    ; Mask to get only C9, C8
    shl dl, 6                       ; Shift to bits 7 and 6
    or cl, dl                       ; Combine with Sector
    
    mov dh, byte [head_temp]        ; Retrieve Head
    
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
    pusha
    mov ah, 0x02                    ; Function: Read Sectors
    mov al, [sectors_this_read]     ; AL = sectors to read
    
    ; Reload DL (drive) just in case
    push ds
    mov ax, 0x0000
    mov ds, ax
    mov dl, [BOOT_DRIVE_ADDRESS]
    pop ds
    
    int 0x13
    popa
    jnc .read_success               ; Success!
    
    ; Read failed, reset disk and retry
    pusha
    
    ; Reload DL (drive) for reset
    push ds
    mov ax, 0x0000
    mov ds, ax
    mov dl, [BOOT_DRIVE_ADDRESS]
    pop ds

    mov ah, 0x00
    int 0x13
    popa
    
    dec cx
    jnz .retry_read
    
    jmp disk_error                  ; All retries failed

.read_success:
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
    
    ; --- Check if we are done *at the end* of the loop ---
    cmp word [sectors_to_read], 0
    jz .a20_stage                   ; All sectors read, jump to A20

    jmp .read_loop                  ; Read next chunk

.a20_stage:
    ; --- Use reliable BIOS call to enable A20 ---
    mov ax, 0x2401              ; Function: Enable A20 Gate
    int 0x15                    ; Call BIOS
    ; --- END FIX ---

    ; --- FIX: LGDT needs the address relative to our ORG ---
    lgdt [gdt_descriptor - 0x1000]
    ; --- END FIX ---

    ; Switch to protected mode by setting the PE bit in CR0
    mov eax, cr0
    or eax, 0x1
    mov cr0, eax

    ; Far jump to our 32-bit code segment (selector 0x08).
    ; This jump also clears the CPU's instruction pipeline.
    ; We use a 32-bit operand size prefix (0x66)
    db 0x66
    jmp 0x08:protected_mode_start

; 16-bit helper routines (No longer needed)
; wait_for_input:
;     in al, 0x64
;     test al, 0x02
;     jnz wait_for_input
;     ret
; 
; wait_for_output:
;     in al, 0x64
;     test al, 0x01
;     jz wait_for_output
;     ret

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