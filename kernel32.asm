; ------------------------------------------------------------------
; Yggdrasil OS - Stage 3 Kernel
;
; This is the 32-bit kernel, loaded at 0x10000.
; It runs in 32-bit Protected Mode.
; ------------------------------------------------------------------

[BITS 32]
org 0x10000 ; We are loaded at physical address 0x10000

kernel_start:
    ; --- 1. Initialize Interrupts ---
    ; This is the most critical step. We must set up our exception
    ; handlers before doing anything else, so if something goes
    ; wrong, we get a message instead of a triple fault.
    cli                     ; Disable interrupts while we set things up
    
    call idt_install        ; Load the IDT register (from idt.asm)
    call isrs_install       ; Populate the IDT with exception handlers (from isrs.asm)
    call PIC_remap          ; Remap the PIC controllers (from pic.asm)
    
    ; --- 2. Setup a proper stack ---
    ; We are currently using the old bootloader stack.
    ; We'll set up a new stack that grows downwards from 0x90000.
    ; This is a safe, unused area of RAM.
    mov esp, 0x90000

    ; --- 3. Clear the screen ---
    ; Video memory is at 0xB8000. Our GDT gives us flat 4GB
    ; access, so we can just write there.
    mov edi, 0xB8000
    mov ecx, 80 * 25
    mov ax, 0x0F20          ; ' ' (space) on White/Black
    rep stosw               ; Fill the screen
    
    ; --- 4. Print a 32-bit welcome message ---
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
    ; --- 5. Enable Interrupts ---
    ; Now that interrupts are set up and remapped, we can 
    ; safely enable them.
    sti
    
    ; To test our new exception handler, uncomment the line below.
    ; It will try to divide by zero, triggering interrupt 0.
    ; div ecx     ; Uncomment to test divide-by-zero exception
    
    hlt         ; Halt the CPU until an interrupt (like keyboard) arrives
    jmp $       ; Infinite loop in case hlt returns

; --- Data ---
welcome_msg: db 'YGGdrasil: Kernel loaded. Interrupts enabled. Waiting for input...', 0

; --- Include our Interrupt Descriptor Table and Interrupt Service Routines ---
; These are now placed *after* the main code, so they are in the data section
; and not executed by mistake.
%include "idt.asm"
%include "isrs.asm"
%include "pic.asm"          ; Include the new PIC code

; --- Padding ---
; Pad to 128KB (256 sectors)
times 131072 - ($ - $$) db 0