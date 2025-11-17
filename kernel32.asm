; kernel32.asm
;
; Stage 3 Kernel Entry Point
; Loaded by Stage 2 at 0x10000.
; Runs in 32-bit Protected Mode.
; Job: Finalize segment registers, set up interrupt system, and run the core loop.
; ------------------------------------------------------------------

[BITS 32]
org 0x10000 ; We are loaded at physical address 0x10000 (KERNEL32_JUMP_ADDRESS)

kernel_start:
    ; --- 1. INITIAL 32-BIT SETUP ---
    ; CS is 0x08 from the RETF jump. Set data segment registers (selector 0x10).
    mov ax, 0x10    ; 0x10 is our Data Segment selector from GDT
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax      ; Set stack segment
    
    ; Set up the stack pointer far away from the kernel code/data
    mov esp, 0x90000 
    
    ; --- 2. BASIC I/O SETUP: Clear Screen ---
    mov edi, 0xB8000 ; Start of Video Memory
    mov ecx, 80 * 25 ; Total character cells (4000)
    mov eax, 0x0F200F20 ; Two characters at once (White space on Black)
    rep stosd        ; Fill the screen with ' '
    
    ; Print 32-bit confirmation message
    mov esi, welcome_msg_32
    mov edi, 0xB8000        ; Top-left
    call print_string_32

    ; --- 3. INTERRUPT SYSTEM SETUP (CRITICAL) ---
    cli                     ; Disable interrupts while setting up control hardware
    
    ; Remap the PIC to map IRQs 0-15 to ISRs 32-47
    call PIC_remap          
    
    ; Load the IDT descriptor into IDTR
    call idt_install        
    
    ; Populate the IDT with exception and IRQ handlers
    call isrs_install       
    
    ; --- 4. KERNEL CHECKPOINT & ENABLE INTERRUPTS ---
    ; Print final checkpoint '9' to confirm interrupt system readiness
    mov edi, 0xB8000 + (3*80*2) ; Move down to line 3
    mov al, '9'
    mov ah, 0x0F
    mov [edi], ax           
    
    sti                     ; Enable hardware interrupts globally
    
    ; Main Loop / Wait for Interrupt
    hlt                     ; Halt the CPU until the next interrupt
    jmp $                   ; Loop indefinitely

; --------------------------------------
; 32-bit Utilities
; --------------------------------------

; Prints a null-terminated string (DS:ESI) to B8000 (VGA Text Mode)
; Preserves EDI as a pointer to the next available screen location.
print_string_32:
    pusha
    mov ebx, edi            ; Save screen buffer address
.loop_str:
    lodsb                   ; Load byte from [ESI] into AL (ESI auto-incremented)
    cmp al, 0
    je .done_str
    
    mov ah, 0x0F            ; Attribute (White on Black)
    stosw                   ; Write AX (char + attr) to [EDI] (EDI auto-incremented by 2)
    jmp .loop_str
.done_str:
    mov edi, ebx            ; Restore screen buffer address
    popa
    ret


; --- Data and Includes ---
welcome_msg_32: db 'SUCCESS! Kernel 32-bit Protected Mode Entered.', 0

; Include dependencies (NASM includes them here during assembly)
%include "idt.asm"
%include "isrs.asm"
%include "pic.asm"

; --- Padding ---
times 131072 - ($ - $$) db 0 ; Pad to 128KB (256 sectors)