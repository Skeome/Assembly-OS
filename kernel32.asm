; kernel32.asm
;
; Stage 3 Kernel Entry Point
; Loaded by Stage 2 at 0x10000.
; Runs in 32-bit Protected Mode.
; Job: Finalize segment registers, clear screen, and prepare for higher level code.
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
    
    ; Stack Pointer setup was critical in 16-bit, but re-init for safety
    mov esp, 0x90000 
    
    ; --- 2. BASIC I/O SETUP ---
    ; Clear the screen by writing spaces (0x20) with white/black attribute (0x0F)
    mov edi, 0xB8000 ; Start of Video Memory
    mov ecx, 80 * 25 ; Total character cells
    mov eax, 0x0F200F20 ; Two characters at once (White space)
    rep stosd        ; Fill the screen
    
    ; Print 32-bit confirmation message
    mov esi, welcome_msg_32
    mov edi, 0xB8000        ; Top-left
    call print_string_32

    ; --- 3. INTERRUPT SYSTEM SETUP ---
    cli                     ; Disable interrupts while setting up control hardware
    call PIC_remap          ; Remap PIC to avoid conflict with CPU exceptions (0-31)
    call idt_install        ; Load the IDT descriptor into IDTR
    call isrs_install       ; Populate the IDT with handlers
    
    ; --- 4. KERNEL CHECKPOINT & HALT ---
    mov edi, 0xB8000 + (2*80) ; Move down one line
    mov al, '9'
    mov ah, 0x0F
    mov [edi], ax           ; Print final checkpoint '9'
    
    sti                     ; Enable hardware interrupts globally
    
    ; Main Loop / Wait for Interrupt
    hlt
    jmp $                   ; Loop indefinitely

; --------------------------------------
; 32-bit Utilities
; --------------------------------------

; Prints a null-terminated string (DS:ESI) to B8000 (VGA Text Mode)
print_string_32:
    pusha
    mov ebx, edi            ; Save screen buffer address
.loop_str:
    lodsb                   ; Load byte from [ESI] into AL (ESI auto-incremented)
    cmp al, 0
    je .done_str
    
    mov ah, 0x0F            ; Attribute (White on Black)
    mov [edi], ax           ; Write AL (char) and AH (attr) to [EDI]
    add edi, 2              ; Move to next cell
    jmp .loop_str
.done_str:
    mov edi, ebx            ; Restore screen buffer address
    popa
    ret


; --- Data and Includes ---
welcome_msg_32: db 'SUCCESS! Kernel 32-bit Protected Mode Entered.', 0

; Include dependencies here (needed for assembly linking)
%include "idt.asm"
%include "isrs.asm"
%include "pic.asm"

; --- Padding ---
times 131072 - ($ - $$) db 0 ; Pad to 128KB (256 sectors)