; ===================================================
; AtlantOS Interrupt Descriptor Table (IDT) & Drivers
; ===================================================

; ---------------------------------------------------------
; 1. INTERRUPT SERVICE ROUTINE (The Handler)
; ---------------------------------------------------------
isr_keyboard:
    pushad                  ; Save registers

    xor eax, eax
    in al, 0x60             ; Read Scan Code

    ; --- 1. CURSOR NAVIGATION ---
    cmp al, 0x4B            ; Left Arrow
    je .move_left
    cmp al, 0x4D            ; Right Arrow
    je .move_right

    ; --- 2. KEY RELEASE CHECK ---
    test al, 0x80           ; If Bit 7 is set, it's a key release
    jnz .send_eoi           ; We ignore releases for now

    ; --- 3. CLI LOGIC (Typing) ---
    ; Handle Backspace (0x0E) specially
    cmp al, 0x0E
    je .handle_backspace

    ; Handle Enter (0x1C) specially
    cmp al, 0x1C
    je .handle_enter

    ; Check bounds of keymap
    cmp al, 58              ; We only mapped up to Space (roughly)
    jg .send_eoi

    ; Translation: Scan Code -> ASCII
    mov ebx, keymap
    add ebx, eax
    mov al, [ebx]           ; Load ASCII char

    cmp al, 0               ; If 0, it's unmapped/special
    je .send_eoi

    call print_char_isr     ; Print it
    jmp .send_eoi

.move_left:
    ; Move cursor BACK 2 bytes (without erasing)
    cmp dword [cursor_pos], 640 ; Start of Line 4
    jle .send_eoi               ; Don't go past prompt start
    sub dword [cursor_pos], 2
    call render_cursor      ; Update cursor visibility
    jmp .send_eoi

.move_right:
    ; Move cursor FORWARD 2 bytes
    cmp dword [cursor_pos], 4000 ; End of Screen
    jge .send_eoi
    add dword [cursor_pos], 2
    call render_cursor      ; Update cursor visibility
    jmp .send_eoi

.handle_backspace:
    call backspace_isr
    jmp .send_eoi

.handle_enter:
    call newline_isr
    jmp .send_eoi

.send_eoi:
    mov al, 0x20
    out 0x20, al            ; Send EOI to Master PIC
    popad
    iretd

; ---------------------------------------------------------
; 2. HELPER FUNCTIONS
; ---------------------------------------------------------
print_char_isr:
    ; AL contains the char to print
    push edi

    mov edi, [cursor_pos]   ; Get current cursor position

    ; Bounds Check: Don't write past screen end
    cmp edi, 4000           ; 80 * 25 * 2
    jge .done_print

    mov [0xB8000 + edi], al      ; Write Character
    mov byte [0xB8000 + edi + 1], 0x0B ; Color: Cyan on Black

    add dword [cursor_pos], 2    ; Advance Cursor
    call render_cursor            ; Update cursor visibility

.done_print:
    pop edi
    ret

backspace_isr:
    push edi

    mov edi, [cursor_pos]

    ; Bounds Check: Don't delete the prompt/status bar
    cmp edi, 640            ; Start of Line 4
    jle .done_back

    sub edi, 2              ; Move cursor back
    mov byte [0xB8000 + edi], ' ' ; Erase char
    mov [cursor_pos], edi   ; Update variable
    call render_cursor      ; Update cursor visibility

.done_back:
    pop edi
    ret

newline_isr:
    push eax
    push edx

    ; Calculate offset to start of next line
    mov eax, [cursor_pos]
    mov edx, 0
    mov ecx, 160        ; 80 chars * 2 bytes
    div ecx             ; EAX = Line Number, EDX = Remainder

    inc eax             ; Next Line
    mul ecx             ; EAX = Start of next line

    mov [cursor_pos], eax
    call render_cursor

    pop edx
    pop eax
    ret

render_cursor:
    push eax
    push edi

    mov edi, [cursor_pos]
    mov al, [0xB8000 + edi + 1]
    mov ah, [cursor_visible]
    test ah, ah
    jz .hide_cursor
    xor al, 0x70        ; For visibility

.hide_cursor:
    mov [0xB8000 + edi + 1], al ; Update cursor visibility
    pop edi
    pop eax
    ret

; ---------------------------------------------------------
; 3. IDT SETUP & REMAP
; ---------------------------------------------------------
setup_idt:
    ; Remap PIC to 0x20..0x27
    mov al, 0x11
    out 0x20, al
    out 0xA0, al
    mov al, 0x20
    out 0x21, al
    mov al, 0x28
    out 0xA1, al
    mov al, 0x04
    out 0x21, al
    mov al, 0x02
    out 0xA1, al
    mov al, 0x01
    out 0x21, al
    out 0xA1, al

    ; Enable Keyboard (IRQ 1)
    mov al, 0xFD
    out 0x21, al
    mov al, 0xFF
    out 0xA1, al

    ; Install ISR
    mov eax, isr_keyboard
    mov [idt_entry_21 + 0], ax
    shr eax, 16
    mov [idt_entry_21 + 6], ax

    lidt [idt_descriptor]
    sti
    ret

; ---------------------------------------------------------
; 4. DATA SECTION
; ---------------------------------------------------------
idt_start:
    times 0x21 dq 0
idt_entry_21:
    dw 0x0000
    dw 0x08
    db 0x00
    db 0x8E
    dw 0x0000
idt_end:
idt_descriptor:
    dw idt_end - idt_start - 1
    dd idt_start

; The Cursor (Points to Video Memory Offset)
cursor_pos: dd 640

; US QWERTY Scan Code Set 1 Map
keymap:
    db 0, 27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', 0x0E, 0    ; 0x00-0x0F
    db 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '[', ']', 0, 0, 'A', 'S'    ; 0x10-0x1F
    db 'D', 'F', 'G', 'H', 'J', 'K', 'L', ';', "'", '`', 0, '\', 'Z', 'X', 'C', 'V'  ; 0x20-0x2F
    db 'B', 'N', 'M', ',', '.', '/', 0, '*', 0, ' '                                  ; 0x30-0x39
    times 100 db 0  ; Padding