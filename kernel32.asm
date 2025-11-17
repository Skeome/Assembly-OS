; ------------------------------------------------------------------
; Yggdrasil OS - Stage 3 Kernel
;
; This is the 32-bit kernel, loaded at 0x10000.
; It runs in 32-bit Protected Mode.
; ------------------------------------------------------------------

[BITS 32]
org 0x10000 ; We are loaded at physical address 0x10000

kernel_start:
    ; --- 0. CRITICAL 32-BIT SETUP (Moved from kernel.asm) ---
    mov esp, 0x90000    ; Set up a safe stack immediately
    
    ; CS is already 0x08 from the RETF.
    ; Set data segments to 0x10.
    mov ax, 0x10    ; 0x10 is our Data Segment selector
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax      ; SS is reset for clarity/safety

    fninit      ; FIX: Initialize FPU 
    
    ; --- 1. Initialize Interrupts (CORRECT ORDER) ---
    cli                     ; Disable interrupts while we set things up

    call PIC_remap          ; 1. Remap the PIC controllers (fixes IRQ race)
    call idt_install        ; 2. Load the IDT register
    call isrs_install       ; 3. Populate the IDT with handlers
    
    
    ; --- 2. Clear the screen ---
    mov edi, 0xB8000
    mov ecx, 80 * 25
    xor eax, eax            ; FIX: Clear all 32 bits of EAX
    mov ax, 0x0F20          ; ' ' (space) on White/Black
    rep stosw               ; Fill the screen
    
    ; --- 3. Print a 32-bit welcome message ---
    mov esi, welcome_msg
    mov edi, 0xB8000        ; Top-left
    
.print_loop:
    lodsb                   ; Load char from [esi] into al
    cmp al, 0
    je .done
    
    mov ah, 0x0F            ; Attribute (White on Black)
    mov [edi], ax
    add edi, 2
    jmp .print_loop
    
.done:
    ; --- 4. Enable Interrupts ---
    sti
    
    hlt         ; Halt the CPU until an interrupt (like keyboard) arrives
    jmp $       ; Infinite loop in case hlt returns

; --- Data ---
welcome_msg: db 'YGGdrasil: Kernel loaded. Interrupts enabled. Waiting for input...', 0

; --- Include our Interrupt Descriptor Table and Interrupt Service Routines ---
%include "idt.asm"
%include "isrs.asm"
%include "pic.asm"

; --- Padding ---
; Pad to 128KB (256 sectors)
times 131072 - ($ - $$) db 0