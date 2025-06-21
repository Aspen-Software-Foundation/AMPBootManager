[bits 16]
[extern kernel_start]
[global BOOT_DRIVE]
[global SCREENADDR]
[global BASEADDR]

UBASE:

jmp _start
db 0
_start:
mov [BOOT_DRIVE], dl
mov [SCREENADDR], ebx
mov eax, ebx
sub eax, 0xB8000
mov [BASEADDR], eax

mov byte [ebx], '!'


CODE_SEG equ GDT_code - GDT_start
DATA_SEG equ GDT_data - GDT_start

cli
lgdt [GDT_descriptor]
mov eax, cr0
or eax, 1
mov cr0, eax
jmp CODE_SEG:start_protected_mode

jmp $
                                    
BOOT_DISK: db 0

GDT_start:
    GDT_null:
        dd 0x0
        dd 0x0

    GDT_code:
        dw 0xffff
        dw 0x0
        db 0x0
        db 0b10011010
        db 0b11001111
        db 0x0

    GDT_data:
        dw 0xffff
        dw 0x0
        db 0x0
        db 0b10010010
        db 0b11001111
        db 0x0

GDT_end:

GDT_descriptor:
    dw GDT_end - GDT_start - 1
    dd GDT_start


[bits 32]
start_protected_mode:
    mov ax, DATA_SEG
    mov ds, ax
    mov ss, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    
    mov ebp, 0x90000        ; 32 bit stack base pointer
    mov esp, ebp
    
    mov byte [ebx], 'H'  ; THIS IS WHERE WE JUMP TO THE KERNEL.... LIKE NO JOKE WE'RE THAT CLOSE RN

    jmp kernel_start

    cli
    hlt




SCREENADDR:
    dd 0

BASEADDR:
    dd 0
    
BOOT_DRIVE:
    db 0