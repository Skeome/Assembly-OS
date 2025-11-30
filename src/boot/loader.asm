; ==================================================================
; AtlantOS - Stage 2 Loader
; The "Gatekeeper" to Protected Mode
; ==================================================================
[ORG 0x1000]          ; We loaded this file here in Stage 1
[BITS 16]           ; Still in 16-bit mode for a moment

loader_start:
    ; Just a quick confirmation we made it here
    mov si, msg_stage2
    call print_string_16

    ; --------------------------------------------
    ; 1. Enable A20 Line
    ; --------------------------------------------
    ; The "A20 Gate" is a legacy quirk. If not enabled, 
    ; we can only access 1MB of RAM. We need it all.
    ; (Simplified Fast A20 method)
    in al, 0x92
    or al, 2
    out 0x92, al

    ; --------------------------------------------
    ; 2. Load Global Descriptor Table (GDT)
    ; --------------------------------------------
    ; The GDT defines memory segments in 32-bit mode.
    cli                     ; Disable interrupts (BIOS interrupts won't work in 32-bit)
    lgdt [gdt_descriptor]   ; Load the GDT pointer

    ; --------------------------------------------
    ; 3. Switch to Protected Mode
    ; --------------------------------------------
    mov eax, cr0            ; Read Control Register 0
    or eax, 0x1             ; Set the PE (Protection Enable) bit
    mov cr0, eax            ; Write it back

    ; --------------------------------------------
    ; 4. The Far Jump
    ; --------------------------------------------
    ; We must perform a "Far Jump" to flush the CPU pipeline 
    ; of any 16-bit instructions and load the Code Segment.
    jmp CODE_SEG:init_32bit

; ==================================================================
; 32-BIT PROTECTED MODE
; ==================================================================
[BITS 32]

init_32bit:
    ; Update Segment Registers to point to our 32-bit Data Segment
    mov ax, DATA_SEG
    mov ds, ax
    mov ss, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    
    ; Move Stack to a safe place at the top of free memory
    mov ebp, 0x90000
    mov esp, ebp

    ; We are now in 32-bit mode. 
    ; We have access to 4GB of RAM and 32-bit registers (EAX, EBX...).
    
    ; Visual Confirmation: Write to Video Memory directly
    ; (Since BIOS interrupts don't work anymore)
    mov byte [0xb8000], 'A'      ; Character 'A'
    mov byte [0xb8001], 0x0B     ; Cyan on Black (Atlantis colors)
    
    ; Jump to Kernel Entry Point
    jmp CODE_SEG:0x2000                  ; Kernel is loaded at 0x2000


; ==================================================================
; DATA & STRUCTURES
; ==================================================================

msg_stage2 db 'Entering the Deep...', 0x0D, 0x0A, 0

; --- GDT Definition ---
gdt_start:

    ; Null Descriptor (Required to be first)
    dd 0x0
    dd 0x0

    ; Code Segment Descriptor
    ; Base=0, Limit=4GB, Executable, Readable
    dw 0xFFFF       ; Limit (bits 0-15)
    dw 0x0000       ; Base (bits 0-15)
    db 0x00         ; Base (bits 16-23)
    db 10011010b    ; Access Byte (Present, Ring 0, Code, Readable)
    db 11001111b    ; Flags (32-bit, 4K granularity) + Limit (bits 16-19)
    db 0x00         ; Base (bits 24-31)

    ; Data Segment Descriptor
    ; Base=0, Limit=4GB, Writable
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 10010010b    ; Access Byte (Present, Ring 0, Data, Writable)
    db 11001111b
    db 0x00

gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1  ; Size of GDT
    dd gdt_start                ; Start address of GDT

; Constants for Segment Offsets
CODE_SEG equ 0x08               ; gdt_start + 1 * 8
DATA_SEG equ 0x10               ; gdt_start + 2 * 8

; Helper function for Stage 2 (16-bit mode)
print_string_16:
    mov ah, 0x0E
.loop:
    lodsb
    or al, al
    jz .done
    int 0x10
    jmp .loop
.done:
    ret