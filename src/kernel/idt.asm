; ===================================================
; AtlantOS Interrupt Descriptor Table (IDT) & Drivers
; ===================================================

; 1. Interrupt Service Routine
; This code runs automatically when a key is pressed
isr_keyboard:
    pushad                  ; Save all general-purpose registers
    ; Acknowledge the hardware
    in al, 0x60             ; Read from the keyboard controller

    ; Ternary Injection Logic
    ; Left Arrow = Negative Entropy
    cmp al, 0x4B
    jne .check_right
    sub esi, 5              ; Inject Negative Entropy
    jmp .send_eoi

.check_right:
    ; Right Arrow = Positive Entropy
    cmp al, 0x4D
    jne .send_eoi
    add esi, 5              ; Inject Positive Entropy

.send_eoi:
    ; Send End of Interrupt (EOI) signal to PIC
    mov al, 0x20
    out 0x20, al            ; Notify PIC that interrupt has been handled
    popad                   ; Restore all general-purpose registers
    iretd                   ; Return from interrupt

; 2. IDT Setup and PIC Remap
setup_idt:
    ; A. Remap PIC
    ; The PIC maps IRQs 0-7 to INT 0x08-0x0F by default
    ; This conflicts with CPU exceptions, so we remap them to 0x20-0x27
    
    ; ICW1: Initialize PICs
    mov al, 0x11
    out 0x20, al            ; Start initialization of PIC1 (Master)
    out 0xA0, al            ; Start initialization of PIC2 (Slave)
    
    ;ICW2: Set vector offsets
    mov al, 0x20            ; Master PIC vector offset starting at 0x20
    out 0x21, al
    mov al, 0x28            ; Slave PIC vector offset starting at 0x28
    out 0xA1, al

    ; ICW3: Setup cascading
    mov al, 0x04            ; Tell Master PIC that Slave PIC is at IRQ2
    out 0x21, al
    mov al, 0x02            ; Tell Slave PIC its cascade identity
    out 0xA1, al

    ; ICW4: Set environment info
    mov al, 0x01            ; 8086/88 mode
    out 0x21, al
    out 0xA1, al

    ; Mask all interrupts except keyboard (IRQ1)
    mov al, 0xFD            ; 1111 1101 (Bit 1 is clear -> Keyboard enabled)
    out 0x21, al
    mov al, 0xFF            ; Mask all on Slave PIC
    out 0xA1, al

    ; B. Populate IDT Entry for keyboard (INT 0x21)
    ; IRQ1 is mapped to INT 0x21 after remap

    mov eax, isr_keyboard   ; Get address of our handler
    mov [idt_entry_21 + 0], ax        ; Lower 16 bits of address
    shr eax, 16
    mov [idt_entry_21 + 6], ax        ; Upper 16 bits of address

    ; C. Load IDT
    lidt [idt_descriptor]

    ; D. Enable Interrupts
    sti                     ; Set Interrupt Flag (CPU will listen to interrupts)
    ret

; 3. IDT Data Structures
idt_start:
    ; We need space for at least 34 interripts (0x00 to 0x21)
    ; To keep it simple, we define a block of empty entries
    ; and manually define the specific ones we need.
    times 0x21 dq 0          ; Empty entries up to INT 0x20

; Keyboard Interrupt Entry (INT 0x21)
idt_entry_21:
    dw 0x0000          ; Offset Low
    dw 0x0008          ; Selector (Kernel Code Segment = 0x08)
    db 0x00            ; Unused, set to 0
    db 10001110b       ; Type and attributes: Present, Ring 0, 32-bit interrupt gate
    dw 0x0000          ; Offset High

idt_end:

idt_descriptor:
    dw idt_end - idt_start - 1  ; Limit (Size of IDT - 1)
    dd idt_start                ; Base address of IDT