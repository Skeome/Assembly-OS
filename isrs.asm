; isrs.asm - Interrupt Service Routines (ISRs) and Interrupt ReQuests (IRQs)
; 
; NOTE: All ISRs and IRQs must run in 32-bit Protected Mode (P-Mode)
; The CPU is expected to switch from the code segment (0x08) to the
; kernel's stack segment (0x10) if the privilege level changes.

; Common stub for all exceptions and interrupts
; When an interrupt or exception occurs, the CPU pushes:
; [EIP, CS, EFLAGS] or [Error Code, EIP, CS, EFLAGS]
isr_common_stub:
    ; Stack layout upon entry to isr_common_stub:
    ; [esp + 20]: EFLAGS
    ; [esp + 16]: CS
    ; [esp + 12]: EIP
    ; [esp + 8]: Error Code (pushed by CPU or manually by macro)
    ; [esp + 4]: Interrupt Number (pushed by macro)
    
    pusha       ; Push eax, ecx, edx, ebx, esp (old), ebp, esi, edi (32 bytes)
    
    ; Stack layout now has the 32-bit interrupt frame + pusha
    
    ; Display a simple "Fault" message
    mov esi, fault_msg
    mov edi, 0xB8000 + (2 * 80 * 2) ; Print on line 2 (4th row from the top)
.fault_loop:
    lodsb
    cmp al, 0
    je .fault_done
    
    mov ah, 0x0C      ; Attribute: Red on Black
    stosw             ; Write AX (char + attr) to [EDI] (EDI auto-incremented by 2)
    jmp .fault_loop
.fault_done:
    
    ; For now, just halt the system
    cli
    hlt

fault_msg: db 'KERNEL PANIC: CPU EXCEPTION. SYSTEM HALTED.', 0

; --- CPU Exception Handlers (ISRs 0-31) ---

%macro ISR_NOERR 1
[GLOBAL isr%1]
isr%1:
    cli                 ; Disable interrupts
    push dword 0        ; Dummy error code (exceptions without error code)
    push dword %1       ; Interrupt number
    jmp isr_common_stub
%endmacro

%macro ISR_ERRCODE 1
[GLOBAL isr%1]
isr%1:
    cli                 ; Disable interrupts
    ; Error code is already on the stack (Pushed by CPU)
    push dword %1       ; Interrupt number
    jmp isr_common_stub
%endmacro

ISR_NOERR 0             ; Divide-by-zero Error
ISR_NOERR 1             ; Debug Exception
ISR_NOERR 2             ; Non-Maskable Interrupt
ISR_NOERR 3             ; Breakpoint
ISR_NOERR 4             ; Overflow
ISR_NOERR 5             ; Bound Range Exceeded
ISR_NOERR 6             ; Invalid Opcode
ISR_NOERR 7             ; Device Not Available (No Math Coprocessor)
ISR_ERRCODE 8           ; Double Fault (Needs error code)
ISR_NOERR 9             ; CoProcessor Segment Overrun (Reserved)
ISR_ERRCODE 10          ; Invalid TSS (Needs error code)
ISR_ERRCODE 11          ; Segment Not Present (Needs error code)
ISR_ERRCODE 12          ; Stack-Segment Fault (Needs error code)
ISR_ERRCODE 13          ; General Protection Fault (GPF)
ISR_ERRCODE 14          ; Page Fault (Needs error code)
ISR_NOERR 15            ; Reserved
ISR_NOERR 16            ; x87 Floating-Point Exception
ISR_ERRCODE 17          ; Alignment Check (Needs error code)
ISR_NOERR 18            ; Machine Check
ISR_NOERR 19            ; SIMD Floating-Point Exception
ISR_NOERR 20            ; Virtualization Exception
ISR_NOERR 21            ; Control Protection Exception
ISR_NOERR 22            ; Reserved
ISR_NOERR 23            ; Reserved
ISR_NOERR 24            ; Reserved
ISR_NOERR 25            ; Reserved
ISR_NOERR 26            ; Reserved
ISR_NOERR 27            ; Reserved
ISR_NOERR 28            ; Hypervisor Injection Exception
ISR_NOERR 29            ; VMM Communication Exception
ISR_ERRCODE 30          ; Security Exception (Needs error code)
ISR_NOERR 31            ; Reserved

; --- Hardware Interrupt Handlers (IRQs 0-15 mapped to ISRs 32-47) ---

%macro IRQ_STUB 2
[GLOBAL irq%1]
irq%1:
    push dword 0x00     ; Push a dummy error code (for alignment)
    push dword (32 + %1) ; Push the final interrupt vector number (32-47)
    
    ; Jump to the common handler, which will save registers and handle EOI
    jmp irq_common_stub
%endmacro

IRQ_STUB 0, 1  ; IRQ 0:  Timer (ISR 32)
IRQ_STUB 1, 1  ; IRQ 1:  Keyboard (ISR 33)
IRQ_STUB 2, 1  ; IRQ 2:  Cascade (ISR 34)
IRQ_STUB 3, 1  ; IRQ 3:  COM2 (ISR 35)
IRQ_STUB 4, 1  ; IRQ 4:  COM1 (ISR 36)
IRQ_STUB 5, 1  ; IRQ 5:  LPT2 (ISR 37)
IRQ_STUB 6, 1  ; IRQ 6:  Floppy Disk (ISR 38)
IRQ_STUB 7, 1  ; IRQ 7:  LPT1 (ISR 39)
IRQ_STUB 8, 2  ; IRQ 8:  RTC (ISR 40)
IRQ_STUB 9, 2  ; IRQ 9:  (ISR 41)
IRQ_STUB 10, 2 ; IRQ 10: (ISR 42)
IRQ_STUB 11, 2 ; IRQ 11: (ISR 43)
IRQ_STUB 12, 2 ; IRQ 12: PS/2 Mouse (ISR 44)
IRQ_STUB 13, 2 ; IRQ 13: FPU/Coprocessor (ISR 45)
IRQ_STUB 14, 2 ; IRQ 14: Primary ATA (ISR 46)
IRQ_STUB 15, 2 ; IRQ 15: Secondary ATA (ISR 47)

; Common IRQ handler logic (32-bit)
irq_common_stub:
    pusha       ; Save registers

    ; At this point, you would call a C/High-level IRQ handler
    ; ...

    ; Send End-of-Interrupt (EOI) signal to the PICs.
    ; Check if IRQ came from Slave PIC (IRQs 8-15)
    mov al, byte [esp + 36] ; Get interrupt number (pushed before pusha)
    cmp al, 40              ; Is it IRQ 8 (40) or higher?
    jl .no_slave_eoi        ; If < 40, skip slave EOI

    ; Send EOI to Slave PIC (port 0xA0)
    mov al, 0x20
    out 0xA0, al      

.no_slave_eoi:
    ; Send EOI to Master PIC (port 0x20)
    mov al, 0x20
    out 0x20, al          

    popa                  ; Restore registers
    
    ; Clean up the stack frame pushed by the stub (Interrupt number + dummy error code)
    add esp, 8
    
    iret                  ; Return from interrupt (Pops EIP, CS, EFLAGS)


; Function to install all the ISRs and IRQs into the IDT (Called from kernel32.asm)
[GLOBAL isrs_install]
isrs_install:
    ; Install CPU Exception ISRs (0-31)
    mov ecx, 0x08     ; Code segment selector
    mov edx, 0x8E     ; Flags: 32-bit interrupt gate, present, DPL=0

    ; --- Install 32 CPU Exceptions (ISRs 0-31) ---
    mov ebx, isr0
    mov eax, 0
    call idt_set_gate
    
    mov ebx, isr1
    mov eax, 1
    call idt_set_gate

    mov ebx, isr2
    mov eax, 2
    call idt_set_gate

    mov ebx, isr3
    mov eax, 3
    call idt_set_gate

    mov ebx, isr4
    mov eax, 4
    call idt_set_gate

    mov ebx, isr5
    mov eax, 5
    call idt_set_gate

    mov ebx, isr6
    mov eax, 6
    call idt_set_gate

    mov ebx, isr7
    mov eax, 7
    call idt_set_gate

    mov ebx, isr8
    mov eax, 8
    call idt_set_gate

    mov ebx, isr9
    mov eax, 9
    call idt_set_gate

    mov ebx, isr10
    mov eax, 10
    call idt_set_gate

    mov ebx, isr11
    mov eax, 11
    call idt_set_gate

    mov ebx, isr12
    mov eax, 12
    call idt_set_gate

    mov ebx, isr13
    mov eax, 13
    call idt_set_gate

    mov ebx, isr14
    mov eax, 14
    call idt_set_gate

    mov ebx, isr15
    mov eax, 15
    call idt_set_gate

    mov ebx, isr16
    mov eax, 16
    call idt_set_gate

    mov ebx, isr17
    mov eax, 17
    call idt_set_gate

    mov ebx, isr18
    mov eax, 18
    call idt_set_gate

    mov ebx, isr19
    mov eax, 19
    call idt_set_gate

    mov ebx, isr20
    mov eax, 20
    call idt_set_gate

    mov ebx, isr21
    mov eax, 21
    call idt_set_gate

    mov ebx, isr22
    mov eax, 22
    call idt_set_gate

    mov ebx, isr23
    mov eax, 23
    call idt_set_gate

    mov ebx, isr24
    mov eax, 24
    call idt_set_gate

    mov ebx, isr25
    mov eax, 25
    call idt_set_gate

    mov ebx, isr26
    mov eax, 26
    call idt_set_gate

    mov ebx, isr27
    mov eax, 27
    call idt_set_gate

    mov ebx, isr28
    mov eax, 28
    call idt_set_gate

    mov ebx, isr29
    mov eax, 29
    call idt_set_gate

    mov ebx, isr30
    mov eax, 30
    call idt_set_gate

    mov ebx, isr31
    mov eax, 31
    call idt_set_gate
    
    ; --- Install 16 IRQ Handlers (ISRs 32-47) ---
    ; IRQs 0-7: Master PIC (32-39)
    mov ebx, irq0
    mov eax, 32
    call idt_set_gate
    
    mov ebx, irq1
    mov eax, 33
    call idt_set_gate
    
    mov ebx, irq2
    mov eax, 34
    call idt_set_gate
    
    mov ebx, irq3
    mov eax, 35
    call idt_set_gate
    
    mov ebx, irq4
    mov eax, 36
    call idt_set_gate
    
    mov ebx, irq5
    mov eax, 37
    call idt_set_gate
    
    mov ebx, irq6
    mov eax, 38
    call idt_set_gate
    
    mov ebx, irq7
    mov eax, 39
    call idt_set_gate
    
    ; IRQs 8-15: Slave PIC (40-47)
    mov ebx, irq8
    mov eax, 40
    call idt_set_gate
    
    mov ebx, irq9
    mov eax, 41
    call idt_set_gate
    
    mov ebx, irq10
    mov eax, 42
    call idt_set_gate
    
    mov ebx, irq11
    mov eax, 43
    call idt_set_gate
    
    mov ebx, irq12
    mov eax, 44
    call idt_set_gate
    
    mov ebx, irq13
    mov eax, 45
    call idt_set_gate
    
    mov ebx, irq14
    mov eax, 46
    call idt_set_gate
    
    mov ebx, irq15
    mov eax, 47
    call idt_set_gate

    ret