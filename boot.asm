[org 0x7C00]
[bits 16]
KERNEL_LOCATION equ 0x7e00    ; Where kernel will be loaded (0x7c00 + 512)
SECTOR_COUNT_MINIMUM equ 0x02         ; Number of sectors to read

SECTOR_COUNT_ISO_PAD equ (SECTOR_COUNT_MINIMUM + 3) / 4
SECTOR_COUNT_READOP equ SECTOR_COUNT_ISO_PAD*4

SECTOR_COUNT_ISO_PAD_BYTES equ SECTOR_COUNT_ISO_PAD*2048

start:
    cli
    mov [BOOT_DRIVE], dl
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00

    mov si, msg_common_1
    call print_si

    cmp dl, 0x80
    jb .floppy
    cmp dl, 0xE0
    jb .hdd
    cmp dl, 0xF0
    jb .cdrom
    jmp .other

.floppy:
    mov si, dev_fdd
    jmp .bldd_common
    
.hdd:
    mov si, dev_hdd
.bldd_common:
    call comncmn
    jmp readsectorsATTEMPT

.cdrom:
    mov si, dev_cdrom
    call comncmn
    jmp no_error

.other:
    mov si, dev_othr
    call comncmn
    jmp halt_b16rm

comncmn:
    call print_si
    mov si, msg_common_2
    call print_si
    call id_device
    ret

print_si:
    lodsb
    test al, al
    jz returncall
    mov ah, 0x0E
    int 0x10
    jmp print_si

id_device:
    mov ah, 0x0E
    mov dl, [BOOT_DRIVE]
print_hex_dl:
    mov al, dl
    shr al, 4
    call .nib
    mov al, dl
    and al, 0x0F
    call .nib
    ret
    
.nib:
    add al, '0'
    cmp al, '9'
    jbe .out
    add al, 7
.out:
    int 0x10
returncall:
    ret

readsectorsATTEMPT:
    mov ah, 0x41               ; Check if LBA is supported
    mov bx, 0x55AA             ; Magic number required by BIOS
    int 0x13                   ; BIOS disk function

    jc .use_CHS                 ; If carry flag is set, LBA is not supported
    jmp .use_CHS                ; If LBA is supported, jump to LBA routine

.use_CHS:                       ; NOTE: I AM USING A LOCAL HERE BCUZ PERHAPS I SHOULD ADD LBA IN THE FUTURE?
    mov bx, KERNEL_LOCATION

    mov ah, 0x02                ; BIOS CHS disk read function
    mov al, SECTOR_COUNT_READOP ; Number of sectors to read
    mov ch, 0x00                ; Cylinder 0
    mov dh, 0x00                ; Head 0
    mov cl, 0x02                ; Sector 2 (first sector is 1)
    mov dl, [BOOT_DRIVE]        ; Boot disk number
    int 0x13                    ; BIOS disk read
    jnc no_error

    mov si, badcode_error_string_1
    mov [ERRDATA], ah

    call print_si
    mov dl, [ERRDATA]
    call print_hex_dl
    jmp halt_b16rm

no_error:
    mov si, msg_bootneardone
    call print_si

mov ah, 0x00               ; BIOS set video mode function
mov al, 0x03               ; Mode 3 = 80x25 text mode
int 0x10                   ; BIOS video interrupt

CODE_SEG equ GDT_code - GDT_start
DATA_SEG equ GDT_data - GDT_start

cli                        ; Disable interrupts before GDT load
lgdt [GDT_descriptor]      ; Load Global Descriptor Table

mov eax, cr0               ; Read CR0 control register
or eax, 1                  ; Set PE bit (bit 0) to enable protected mode
mov cr0, eax               ; Write back to CR0

jmp CODE_SEG:start_protected_mode ; Far jump to reload CS and enter protected mode

halt_b16rm:
    cli
    hlt
    jmp halt_b16rm

; GDT definition for protected mode
GDT_start:
    GDT_null:
        dd 0x0
        dd 0x0

    GDT_code:
        dw 0xffff            ; Segment limit (max 64KB)
        dw 0x0               ; Base low
        db 0x0               ; Base middle
        db 0b10011010        ; Access byte (code segment, executable, readable, accessed)
        db 0b11001111        ; Flags (granularity, 32-bit)
        db 0x0               ; Base high

    GDT_data:
        dw 0xffff
        dw 0x0
        db 0x0
        db 0b10010010        ; Access byte (data segment, writable)
        db 0b11001111        ; Flags
        db 0x0

GDT_end:

GDT_descriptor:
    dw GDT_end - GDT_start - 1  ; Size of GDT - 1
    dd GDT_start                ; Address of GDT

[bits 32]
start_protected_mode:
    jmp KERNEL_LOCATION       ; Jump to kernel loaded at 0x7e00 (assumed to be 32-bit code)

HALT_PM_B32:
    cli
    hlt
    jmp HALT_PM_B32

BOOT_DRIVE db 0
ERRDATA db 0

msg_common_1 db 'BOOT ', 0
msg_common_2 db 13, 10, 'DISK ID: 0x',0

dev_fdd   db 'FDD',0
dev_hdd   db 'HDD',0
dev_cdrom db 'CDROM',0
dev_othr  db 'Other Device',0

badcode_error_string_1 db 13, 10, 'ERR: 0x0BCE', 0
badcode_error_string_2 db 13, 10, 'ERR: 0x1BCE', 0
msg_bootneardone db 13, 10, 'NBT', 0

;align 16
;lba_packet:
;    db 16
;    db 0
;    dw SECTOR_COUNT_READOP
;    dd KERNEL_LOCATION
;    dw 0          ; segment
;    dw 0          ; reserved
;    dq 1          ; start LBA



times 446 - ($ - $$) db 0

times 510 - ($ - $$) db 0
dw 0xAA55

mov ax, DATA_SEG          ; Load data segment selector
mov ds, ax
mov ss, ax
mov es, ax
mov fs, ax
mov gs, ax

mov ebp, 0x90000		  ; Set 32-bit stack base pointer
mov esp, ebp
jmp bootdone


msg_bootdone db 'WELCOME TO ASPEN MULTI-PLATFORM OPERATING SYSTEM (AMPOS) BOOT MANAGER (AMPBootMgr)', 0

bootdone:
    mov ebx, 0xB8000
    mov ecx, 0

.loop:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0C
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop

.done:
    jmp HALT_PM_B32

times SECTOR_COUNT_ISO_PAD_BYTES - ($ - $$) db 0
