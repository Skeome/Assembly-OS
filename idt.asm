; ------------------------------------------------------------------
; Yggdrasil OS - Interrupt Descriptor Table (IDT)
; ------------------------------------------------------------------
; The IDT is loaded at 0x10000 + offset (within kernel32.asm)
; Since kernel32.asm is padded to 128KB, the IDT is within this region.

IDT_SIZE equ 256 ; Maximum number of entries

; Reserve space for the IDT
; An IDT entry is 8 bytes long: 256 * 8 = 2048 bytes
idt:
    times IDT_SIZE * 8 db 0  ; Reserve space, initialized to zero

; The IDT descriptor (pointer) used by the 'lidt' instruction
idt_descriptor:
    dw (IDT_SIZE * 8) - 1  ; Limit (size of IDT in bytes - 1)
    dd idt                 ; Base (linear address of idt - updated at runtime)


; Function: idt_set_gate
; Description: Sets a single IDT gate entry (32-bit interrupt gate).
; ARGS:
;   eax: interrupt number (0-255)
;   ebx: 32-bit address of the interrupt handler
;   ecx: 16-bit code segment selector (e.g., 0x08)
;   edx: 16-bit flags (e.g., 0x8E00 for 32-bit interrupt gate)
[GLOBAL idt_set_gate]
idt_set_gate:
    push edi
    push eax
    push ebx ; Save EBX before it's modified for the high 16-bit calculation
    push ecx
    push edx

    ; edi = idt + (interrupt_number * 8)
    shl eax, 3  ; eax = interrupt_number * 8
    lea edi, [idt + eax]
    
    ; 1. Set Offset (low 16 bits)
    mov [edi], bx               ; Write low 16 bits of handler address (uses 16-bit BX)
    
    ; 2. Set Selector (2 bytes)
    mov [edi + 2], cx           ; Write segment selector (e.g. 0x08)
    
    ; 3. Set Reserved byte and Flags/Type (2 bytes)
    ; Flags are in EDX, but we only use the low 16 bits (0x8E00 or 0x8E)
    mov [edi + 4], dl           ; Write low byte of flags (P, DPL, S, Type)
    
    ; 4. Set Offset (high 16 bits)
    ; Fix: Use EAX as temp register to calculate and write the high 16 bits
    mov eax, [esp + 12]         ; EAX = full 32-bit address (EBX was pushed 4th, so it's at esp+12 after 4 pushes)
    shr eax, 16                 ; EAX = high 16 bits (clear low 16 bits)
    mov [edi + 6], ax           ; Write high 16 bits of handler address (using 16-bit AX)

    pop edx
    pop ecx
    pop ebx
    pop eax
    pop edi
    ret

; Function to load the IDT
[GLOBAL idt_install]
idt_install:
    ; CRITICAL FIX: The IDT base address is relative to ORG 0x10000.
    ; We must add the load address (0x10000) to the IDT's base address pointer.
    mov ebx, idt          ; Get IDT label offset
    add ebx, 0x10000      ; Add the load address 0x10000 (Physical address of kernel32.asm)
    mov dword [idt_descriptor + 2], ebx ; Update the 32-bit base address field of the descriptor
    
    ; Load the IDT descriptor into the IDTR register
    lidt [idt_descriptor]
    ret