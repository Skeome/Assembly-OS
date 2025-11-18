; kernel32.asm
;
; Stage 3 Kernel Entry Point
; Loaded by Stage 2 at 0x10000.
; Runs in 32-bit Protected Mode.
; Job: Finalize segment registers, clear screen, and display final message.
; ------------------------------------------------------------------

[BITS 32]
org 0x10000 ; We are loaded at physical address 0x10000

kernel_start:
    ; --- 1. INITIAL 32-BIT SETUP ---
    ; CS is 0x08 from the RETF jump. Set data segment registers (selector 0x10).
    mov ax, 0x10    ; 0x10 is our Data Segment selector from GDT
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax      ; Set stack segment
    
    ; Set up the stack pointer 
    mov esp, 0x400000 ; FIXED: Set stack pointer to a very safe, high address (4MB)
    
    ; --- 2. CLEAR SCREEN (VGA Text Mode) ---
    mov edi, 0xB8000        ; Start of Video Memory (Physical address)
    mov ecx, 80 * 25        ; Total character cells (4000)
    mov eax, 0x0F200F20     ; Two characters at once (White space on Black)
    rep stosd               ; Fill the screen
    
    ; --- 3. PRINT WELCOME MESSAGE ---
    mov esi, welcome_msg_32
    mov edi, 0xB8000        ; Top-left
    call print_string_32
    
    ; --- 4. HALT ---
    cli
    hlt                     
    jmp $                   

; --------------------------------------
; 32-bit Utilities
; --------------------------------------

; Prints a null-terminated string (DS:ESI) to B8000 (VGA Text Mode)
print_string_32:
    pusha
    mov ebx, edi            ; Save screen buffer address
.loop_str:
    lodsb                   ; Load byte from [DS:ESI] into AL 
    cmp al, 0
    je .done_str
    
    mov ah, 0x0F            ; Attribute (White on Black)
    stosw                   ; Write AX (char + attr) to [EDI] 
    jmp .loop_str
.done_str:
    mov edi, ebx            ; Restore screen buffer address
    popa
    ret


; --- Data ---
welcome_msg_32: db 'Protected Mode Active: Kernel Core Initialized.', 0

; --- Padding ---
times 131072 - ($ - $$) db 0 ; Pad to 128KB (256 sectors)