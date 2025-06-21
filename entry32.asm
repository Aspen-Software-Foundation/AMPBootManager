[bits 32]
[global kernel_start]
[extern kernel_main]
[extern BOOT_DRIVE]
[extern SCREENADDR]
[extern BASEADDR]

kernel_start:
    mov byte [ebx],   'Q'  ; THIS IS WHERE WE JUMP TO THE KERNEL.... LIKE NO JOKE WE'RE THAT CLOSE RN
    mov byte [ebx+2], 'E'  ; ... THIS IS WHERE WE JUMP TO THE KERNEL.... LIKE NO JOKE WE'RE THAT CLOSE RN
    mov byte [ebx+4], 'M'  ; ...... THIS IS WHERE WE JUMP TO THE KERNEL.... LIKE NO JOKE WE'RE THAT CLOSE RN
    mov byte [ebx+6], 'U'  ; ......... Okay that's enough

    mov dl, [BOOT_DRIVE]

    call kernel_main


full_halt:   ; IF kernel returns-- Uh oh (Basically BSOD)
    cli
    hlt
    jmp full_halt