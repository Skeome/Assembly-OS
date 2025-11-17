; ------------------------------------------------------------------
; Yggdrasil OS - Interrupt Descriptor Table (IDT)
; ------------------------------------------------------------------

; Structure for an IDT entry (gate) - for reference
; An IDT entry is 8 bytes long.
; 0-1: Base (low)
; 2-3: Selector
; 4:   Reserved (always 0)
; 5:   Flags
; 6-7: Base (high)

; We need 256 entries for all possible interrupts (0-255)
IDT_SIZE equ 256

; Reserve space for the IDT
; We will fill this table using the idt_set_gate function
idt:
    times IDT_SIZE * 8 db 0  ; Reserve 256 * 8 = 2048 bytes, initialized to zero

; The IDT descriptor (pointer) used by the 'lidt' instruction
idt_descriptor:
    dw (IDT_SIZE * 8) - 1  ; Limit (size of IDT in bytes - 1)
    dd idt                 ; Base (linear address of idt)


; Function to set up a single IDT gate
; ARGS:
;   eax: interrupt number (0-255)
;   ebx: address of the interrupt handler
;   ecx: code segment selector (e.g., 0x08)
;   edx: flags (e.g., 0x8E for 32-bit interrupt gate)
idt_set_gate:
    push edi
    push eax
    push ebx

    ; edi = idt + (interrupt_number * 8)
    shl eax, 3  ; eax = interrupt_number * 8
    lea edi, [idt + eax]
    
    ; Set base address (low 2 bytes)
    mov [edi], bx
    ; Set base address (high 2 bytes)
    shr ebx, 16
    mov [edi + 6], bx
    
    ; Set selector (2 bytes)
    mov [edi + 2], cx
    
    ; Set reserved byte (1 byte)
    mov byte [edi + 4], 0
    
    ; Set flags (1 byte)
    mov [edi + 5], dl

    pop ebx
    pop eax
    pop edi
    ret

; Function to load the IDT
idt_install:
    ; Load the IDT descriptor into the IDTR register
    lidt [idt_descriptor]
    ret