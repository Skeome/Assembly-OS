; ------------------------------------------------------------------
; Yggdrasil OS - Stage 3 Kernel
;
; This is the 32-bit kernel, loaded at 0x10000.
; It runs in 32-bit Protected Mode.
; ------------------------------------------------------------------

[BITS 32]
org 0x10000 ; We are loaded at physical address 0x10000

kernel_start:
    ; --- 0. 32-BIT SETUP (Fixed: ESP should now be correct from kernel.asm) ---
    mov esp, 0x90000    ; Reset up a safe stack immediately
    
    ; CS is already 0x08 from the RETF.
    ; Set data segments to 0x10.
    mov ax, 0x10    ; 0x10 is our Data Segment selector
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax      ; SS is reset for clarity/safety

    fninit      ; Initialize FPU
    cli         ; Disable interrupts while we set things up

    ; --- 1. Initialize Interrupts ---
    call PIC_remap          ; 1. Remap the PIC controllers (fixes IRQ race)
    call idt_install        ; 2. Load the IDT register
    call isrs_install       ; 3. Populate the IDT with handlers
    
    ; --- REMOVED: wait_for_keypress_poll call (Moved to kernel.asm) ---
    
    ; --- 2. Clear the screen ---
    ; NOTE: The infinite '8(' was likely occurring before this point due to a GPF/PF
    ; caused by the uninitialized ESP from kernel.asm. This is the new, clean screen.
    mov edi, 0xB8000
    mov ecx, 80 * 25
    xor eax, eax            ; Clear all 32 bits of EAX
    mov ax, 0x0F20          ; ' ' (space) on White/Black
    rep stosw               ; Fill the screen
    
    ; --- 3. Print a 32-bit welcome message ---
    mov esi, welcome_msg
    mov edi, 0xB8000        ; Top-left

.print_loop:        ; Corrected Print Loop
    lodsb                   ; Load char from [esi] into al (ESI is auto-incremented)
    cmp al, 0
    je .done_printing
    
    mov ah, 0x0F            ; Attribute (White on Black)
    mov [edi], ax           ; Write character (AL) and attribute (AH) to screen
    add edi, 2              ; Move to next character cell (2 bytes)
    jmp .print_loop
    
.done_printing:
    
    ; --- 4. Enable Interrupts and Wait ---
    
    ; Unmask the Keyboard IRQ (IRQ 1 is at bit 1 of PIC1_DATA 0x21)
    in al, 0x21
    and al, 0b11111101 ; Clear the IRQ1 mask bit
    out 0x21, al
    
    sti                 ; Enable interrupts globally
    
    ; Print final message
    mov esi, final_msg
    mov edi, 0xB8000 + (2*80) ; Print on the second line
.final_print_loop:
    lodsb
    cmp al, 0
    je .final_done
    
    mov ah, 0x0E ; Yellow on Black
    mov [edi], ax
    add edi, 2
    jmp .final_print_loop
.final_done:
    
    ; --- Halt: The main OS loop ---
    hlt         ; Halt the CPU until an interrupt (like keyboard or timer) arrives
    jmp $       ; Infinite loop in case hlt returns


; --- Data ---
welcome_msg: db 'YGGdrasil: Kernel loaded and stabilized in 32-bit Protected Mode.', 0
final_msg:   db 'IRQs enabled. Waiting for input (Timer/Keyboard).', 0

; --- Include our Interrupt Descriptor Table and Interrupt Service Routines ---
%include "idt.asm"
%include "isrs.asm"
%include "pic.asm"

; --- Padding ---
; Pad to 128KB (256 sectors)
times 131072 - ($ - $$) db 0