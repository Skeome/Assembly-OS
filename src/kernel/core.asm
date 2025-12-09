; ==================================================================
; AtlantOS - Core Kernel (Simulated Ternary Logic)
; ==================================================================
[BITS 32]
[ORG 0x2000]        ; We will load the kernel at 0x2000

kernel_entry:
    ; Clear the screen (Simple loop)
    mov edi, 0xB8000
    mov ecx, 80 * 25
    mov ax, 0x0720      ; Black background, white text, space character
    rep stosw

    ; Disable hardware cursor
    mov dx, 0x3D4       ; CRT Controller Index Register
    mov al, 0x0A        ; Cursor Start Register
    out dx, al
    mov dx, 0x3D5       ; CRT Controller Data Register
    mov al, 0x20        ; Disable cursor (bits 5-0: start scanline)
    out dx, al

    mov dx , 0x3D4  
    mov al, 0x0B        ; Cursor End Register
    out dx, al
    mov dx, 0x3D5
    mov al, 0x00        ; disable cursor (bits 5-0: end scanline)
    out dx, al

    ; Setup the "Ternary Test"
    ; We will start with a positive drift and watch it stabilize
    ;mov eax, 40         ; Removed: Start with positive drift
    mov esi, 0           ; EAX represents Sysyem Error/Entropy state
    mov edi, 0xB8000     ; Start writing at top-left

    ; Call IDT Setup Routine
    call setup_idt

kernel_loop:
    ; ---------------------------------------------------------
    ; VISUALIZATION
    ; ---------------------------------------------------------
    ; We need to save registers because we are about to do math
    push esi
    push edi             

    ; Reset render pointer to the start of Status Line
    mov edi, 0xB8000

    ; Clear the status line (Erase previous state)
    mov ecx, 80
    mov ax, 0x0720      ; Space character with default color
    rep stosw

    ; Reset render pointer again
    mov edi, 0xB8000
    
    ; Default character: 'X' (Zero/Locked)
    mov cx, 0x0A58      ; 'X' character with Light Green color
    cmp esi, 0
    je .draw_status            ; If zero, keep 'X'

    ; If not zero, assume Negative first
    mov cx, 0x0C3C    ; '<' character with Light Red color
    cmp esi, 0
    jl .draw_status            ; If less than 0, keep '<'

    ; Must be positive
    mov cx, 0x0E3E    ; '>' character with Light Yellow color

.draw_status:
    ; Render the state bar (Visualizing the magnitude of error)
    ; We map the value in EAX to a position on the screen
    mov ebx, esi        ; Copy Drift Value
    cmp ebx, 0
    jge .abs_calc
    neg ebx             ; Get absolute value if negative
.abs_calc:
    ; If stable (0), draw single indicator
    cmp ebx, 0
    jne .render_loop
    mov [edi], cx       ; Write char+color to video memory
    jmp .update_labels

.render_loop:
    cmp ebx, 0
    je .update_labels
    mov [edi], cx       ; Write status char+color
    add edi, 2          ; Move to next screen cell
    dec ebx
    jmp .render_loop

.update_labels:
    ;---------------------------------------------------------
    ; Kernel Status Messages
    ;---------------------------------------------------------
    mov edi, 0xB80A0    ; Move to start of Line 2 (Row 1, Col 0)

    cmp esi, 0
    jne .status_correcting

    ; STATUS: STABLE (Cyan)
    mov dword [edi], 0x0B540B53   ; ST
    mov dword [edi+4], 0x0B420B41 ; AB
    mov dword [edi+8], 0x0B450B4C ; LE
    jmp .ternary_logic

.status_correcting:
    ; STATUS: ACTIVE (Red - Kernel is working)
    mov dword [edi], 0x0C430C41   ; AC
    mov dword [edi+4], 0x0C490C54 ; TI
    mov dword [edi+8], 0x0C450C56 ; VE

.ternary_logic:
    pop edi              ; Restore EDI
    pop esi              ; Restore ESI

    ; ---------------------------------------------------------
    ; Ternary Scheduler Logic
    ; ---------------------------------------------------------
    ; The kernel automatically seeks equilibrium (0)
    ; In a full OS, this logic would balance load between cores.

    cmp esi, 0
    je .cycle_delay

    ; Branchless Ternary Operator
    ; Direction = sign(ESI)
    xor ebx, ebx
    xor edx, edx
    cmp esi, 0
    setg bl                 ; BL = 1 if Positive
    setl dl                 ; DL = 1 if Negative
    sub ebx, edx            ; EBX = 1 or -1

    sub esi, ebx            ; Restore Equilibrium

    ; ---------------------------------------------------------
    ; Kernel Cycle Delay
    ; ---------------------------------------------------------

.cycle_delay:
    mov ecx, 0x00400000
.wait:
    loop .wait

    jmp kernel_loop

; ==================================================================
; Include Drivers
; ==================================================================
    %include 'src/kernel/idt.asm'