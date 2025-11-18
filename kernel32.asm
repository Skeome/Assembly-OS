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
    ; --- 1. INITIAL 32-BIT SETUP (MUST RELOAD ALL SEGMENTS!) ---
    ; CS is 0x08 from the far jump. Now reload all data segment registers (selector 0x10).
    mov ax, 0x10    ; 0x10 is our Data Segment selector from GDT (Base 0, Limit 4GB)
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax      ; Set Stack Segment
    
    ; Set up the stack pointer 
    ; NOTE: The stack is set to a safe, high address (4MB) to avoid conflicting with lower memory areas.
    mov esp, 0x400000 
    
    ; --- 2. CLEAR SCREEN (VGA Text Mode) ---
    ; DS and ES are now 0x10 (flat 4GB segment). ES is used implicitly by stosd.
    mov edi, 0xB8000        ; Destination is Video Memory (0xB8000 physical)
    mov ecx, 80 * 25        ; Total character cells (4000)
    mov eax, 0x0F200F20     ; White space on Black (Color 0x0F, Char 0x20)
    rep stosd               ; Fill the screen
    
    ; --- 3. PRINT WELCOME MESSAGE ---
    mov esi, welcome_msg_32 ; Source string offset (relative to DS base 0)
    mov edi, 0xB8000        ; Destination is Video Memory
    call print_string_32
    
    ; --- 4. HALT ---
    cli
    hlt                     
    jmp $                   

; --------------------------------------
; 32-bit Utilities
; --------------------------------------

; Prints a null-terminated string (DS:ESI) to B8000 (VGA Text Mode)
; Uses ES for destination (Video Memory) and DS for source (Kernel Data)
print_string_32:
    pusha
    mov ebx, edi            ; Save screen buffer address (0xB8000)
.loop_str:
    lodsb                   ; Load byte from [DS:ESI] into AL (Source is kernel code/data)
    cmp al, 0
    je .done_str
    
    mov ah, 0x0F            ; Attribute (White on Black)
    stosw                   ; Write AX (char + attr) to [ES:EDI] (Destination is B8000)
    jmp .loop_str
.done_str:
    mov edi, ebx            ; Restore screen buffer address
    popa
    ret


; --- Data ---
welcome_msg_32: db 'Protected Mode Active: Kernel Core Initialized.', 0

; --- Padding ---
times 131072 - ($ - $$) db 0 ; Pad to 128KB (256 sectors)