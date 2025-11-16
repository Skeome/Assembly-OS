; pic.asm - Functions to initialize and remap the 8259 PIC
; This moves hardware interrupts (IRQs 0-15) to vectors 32-47,
; which prevents conflicts with CPU exceptions (0-31).

%define PIC1_CMD 0x20
%define PIC1_DATA 0x21
%define PIC2_CMD 0xA0
%define PIC2_DATA 0xA1

%define ICW1_INIT 0x10      ; Initialization command
%define ICW1_ICW4 0x01      ; Expect a 4th Initialialization Command Word
%define ICW4_8086 0x01      ; 8086/88 (MCS-80/85) mode

%define PIC1_START_VECTOR 0x20  ; Remap IRQ 0-7 to vectors 32-39 (0x20-0x27)
%define PIC2_START_VECTOR 0x28  ; Remap IRQ 8-15 to vectors 40-47 (0x28-0x2F)

; Function: PIC_remap
; Description: Remaps the PIC controllers.
PIC_remap:
    push eax
    
    ; --- Start initialization sequence (ICW1) ---
    mov al, ICW1_INIT | ICW1_ICW4 ; 0x11
    out PIC1_CMD, al              ; Send to Master PIC
    call io_wait                  ; Short delay
    out PIC2_CMD, al              ; Send to Slave PIC
    call io_wait

    ; --- ICW2: Set vector offsets ---
    mov al, PIC1_START_VECTOR     ; Master PIC vector offset
    out PIC1_DATA, al
    call io_wait
    mov al, PIC2_START_VECTOR     ; Slave PIC vector offset
    out PIC2_DATA, al
    call io_wait

    ; --- ICW3: Tell Master PIC about Slave (on IRQ2) ---
    mov al, 0x04                  ; 00000100b - Slave is on IRQ 2
    out PIC1_DATA, al
    call io_wait

    ; --- ICW3: Tell Slave PIC its cascade identity (2) ---
    mov al, 0x02                  ; 00000010b - Corresponds to IRQ 2
    out PIC2_DATA, al
    call io_wait

    ; --- ICW4: Set mode (8086) ---
    mov al, ICW4_8086
    out PIC1_DATA, al
    call io_wait
    out PIC2_DATA, al
    call io_wait

    ; --- Mask all interrupts (disable them) ---
    ; We will unmask them one by one later as we
    ; write drivers for them.
    mov al, 0xFF    ; 1 = masked/disabled
    out PIC1_DATA, al
    out PIC2_DATA, al
    
    pop eax
    ret

; A small delay function for older hardware.
; 'out 0x80, al' is a classic way to do this. Port 0x80 is
; unused and safe to write to.
io_wait:
    out 0x80, al
    ret