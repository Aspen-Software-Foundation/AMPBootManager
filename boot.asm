[org 0x7C00]
[bits 16]
KERNEL_LOCATION equ 0x7e00    ; Where kernel will be loaded (0x7c00 + 512)
SECTOR_COUNT_MINIMUM equ 0x02         ; Number of sectors to read

SECTOR_COUNT_ISO_PAD equ (SECTOR_COUNT_MINIMUM + 3) / 4
SECTOR_COUNT_READOP equ (SECTOR_COUNT_ISO_PAD*4)-1

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
    jmp continue_boot

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



; Make A function Elsewhere (AFTER the bootsig)
readsectorsATTEMPT:
    mov ah, 0x41               ; Check if LBA is supported
    mov bx, 0x55AA             ; Magic number required by BIOS
    int 0x13                   ; BIOS disk function

    jc .use_CHS                 ; If carry flag is set, LBA is not supported
    jmp .use_LBA                ; If LBA is supported, jump to LBA routine

.use_CHS:                       ; NOTE: I AM USING A LOCAL HERE BCUZ PERHAPS I SHOULD ADD LBA IN THE FUTURE?
    mov bx, KERNEL_LOCATION

    mov ah, 0x02                ; BIOS CHS disk read function
    mov al, SECTOR_COUNT_READOP ; Number of sectors to read
    mov ch, 0x00                ; Cylinder 0
    mov dh, 0x00                ; Head 0
    mov cl, 0x02                ; Sector 2 (first sector is 1)
    mov dl, [BOOT_DRIVE]        ; Boot disk number
    int 0x13                    ; BIOS disk read
    jnc .no_error

    mov si, badcode_error_string_1
    jmp .errcommon
    
.use_LBA:
    mov si, dap     ; SI gets offset
    mov bx, si      ; BX = offset of DAP (within DS)
    
.reset:
    mov dl, [BOOT_DRIVE]
	mov ah, 0	; reset disk
	int 0x13
	jc .reset	; reset again if error

    mov ah, 0x42                 ; Extended read
    mov dl, [BOOT_DRIVE]
    int 0x13
    jc .reset
    jmp .no_error             ; jump to loaded kernel

    mov si, badcode_error_string_1

.errcommon:
    mov [ERRDATA], ah

    call print_si
    mov dl, [ERRDATA]
    call print_hex_dl
    jmp halt_b16rm


.no_error:
    ; Would return in a function but no func so just continue boot
continue_boot:
    mov si, msg_bootneardone
    call print_si

jmp KERNEL_LOCATION

halt_b16rm:
    cli
    hlt
    jmp halt_b16rm

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

align 16
dap:
    db 16                   ; size of DAP (16 bytes)
    db 0                    ; reserved (1 byte)
    dw SECTOR_COUNT_READOP  ; Operation size (like in NASM when u move data without saying word or byte, but in sectors and for reading disks)
    dw KERNEL_LOCATION      ; offset (BX)
    dw 0x0000               ; segment (ES)
    dd 1                    ; Lower 32 bits of starting sector
    dd 0                    ; Upper 32 bits of starting sector (almost always 0)



times 446 - ($ - $$) db 0
; MBR PARTITION TABLE HERE!!!!!!!!!!111!!!!
times 510 - ($ - $$) db 0
dw 0xAA55

jmp bootdone

tempdata:
    .reg_eax dd 0
    .reg_ebx dd 0
    .reg_ecx dd 0
    .reg_edx dd 0

    
read_drive_registers:
    .reg_eax dd 0
    .reg_ebx dd 0
    .reg_ecx dd 0
    .reg_edx dd 0

SECTORS_PER_TRACK:
    dw 0x12 ; idk maybe there would be more than 255 or some shi
lba2chs:
    push bp
    mov bp, sp
    push ax
    push bx
    push dx
    sub sp, 2 * 3        ; -2 = HEAD
                ; -4 = TRACK
                ; -6 = SECTOR
 
    mov si, [bp + 4] ; Linear Block Address

    ; compute HEAD
    mov cx, SECTORS_PER_TRACK
    shl cx, 1            ; SPT * 2
    xor dx, dx            ; initialize quotient
    mov ax, si            ; LBA % 
    mov bx, cx            ; (SPT * 2)
    div bx

    mov ax, dx            ; use quotient as next dividend
    xor dx, dx
    mov bx, SECTORS_PER_TRACK    ; / SPT
    div bx
    mov [bp - 2], ax        ; store HEAD (remainder)

    ; compute TRACK
    xor dx, dx               ; initialize quotient
    mov ax, si            ; LBA %
    mov bx, cx            ; (SPT * 2)
    div bx
    mov [bp - 4], ax        ; store TRACK (remainder)

    ; compute SECTOR
    xor dx, dx
    mov ax, si
    mov bx, SECTORS_PER_TRACK
    div bx
    add dx, 1
    mov [bp - 6], dx        ; store SECTOR (quotient)

    pop dx
    pop bx
    pop ax

    ; assemble return values
    mov dh, byte [bp - 2]        ; load HEAD
    mov ch, byte [bp - 4]        ; load TRACK
    mov cl, byte [bp - 6]        ; load SECTOR
    mov sp, bp
    pop bp
    ret

; PLACE DISK READ FUNCTION HERE
DiskRead16bRM:
    pusha
    mov ah, 0x41               ; Check if LBA is supported
    mov bx, 0x55AA             ; Magic number required by BIOS
    int 0x13                   ; BIOS disk function

    jc .use_CHS                 ; If carry flag is set, LBA is not supported
    jmp .use_LBA                ; If LBA is supported, jump to LBA routine

.use_CHS:                       ; NOTE: I AM USING A LOCAL HERE BCUZ PERHAPS I SHOULD ADD LBA IN THE FUTURE?
    mov bx, KERNEL_LOCATION

    mov ah, 0x02                ; BIOS CHS disk read function
    mov al, SECTOR_COUNT_READOP ; Number of sectors to read
    mov ch, 0x00                ; Cylinder 0
    mov dh, 0x00                ; Head 0
    mov cl, 0x02                ; Sector 2 (first sector is 1)
    mov dl, [BOOT_DRIVE]        ; Boot disk number
    int 0x13                    ; BIOS disk read
    jnc .done

    mov si, badcode_error_string_1
    jmp .errcommon
    
.use_LBA:
    mov si, dap     ; SI gets offset
    mov bx, si      ; BX = offset of DAP (within DS)
    
.reset:
    mov dl, [BOOT_DRIVE]
	mov ah, 0	; reset disk
	int 0x13
	jc .reset	; reset again if error

    mov ah, 0x42                 ; Extended read
    mov dl, [BOOT_DRIVE]
    int 0x13
    jc .reset
    jmp .done             ; jump to loaded kernel
.errcommon:
    mov [ERRDATA], ah

    call print_si
    mov dl, [ERRDATA]
    call print_hex_dl
    stc

.done:
    ret

align 16
.dap:
    db 16                   ; size of DAP (16 bytes)
    db 0                    ; reserved (1 byte)
    dw SECTOR_COUNT_READOP  ; Operation size (like in NASM when u move data without saying word or byte, but in sectors and for reading disks)
    dw KERNEL_LOCATION      ; offset (BX)
    dw 0x0000               ; segment (ES)
    dd 1                    ; Lower 32 bits of starting sector
    dd 0                    ; Upper 32 bits of starting sector (almost always 0)


msg_bootdone db 'WELCOME TO ASPEN MULTI-PLATFORM OPERATING SYSTEM (', 0
db 'AMPOS', 0
db ') BOOT MANAGER (', 0
db 'AMPBootMgr', 0
db ')', 0

HALT_PM_B32:
    cli
    hlt
    jmp HALT_PM_B32

bootdone:
    mov ah, 0x00               ; BIOS set video mode function
    mov al, 0x03               ; Mode 3 = 80x25 text mode
    int 0x10                   ; BIOS video interrupt

    mov ebx, 0xB8000
    mov ecx, 0

.loop0:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0C
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop0
    
    inc ecx
.loop1:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0D
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop1
    
    inc ecx
.loop2:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0C
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop2
    
    inc ecx
.loop3:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0D
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop3
    
    inc ecx
.loop4:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0C
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop4

.done:
    jmp HALT_PM_B32


times SECTOR_COUNT_ISO_PAD_BYTES - ($ - $$) db 0


; 69 20 6D 65 61 6E 74 20 69 20 77 61 73 20 67 6F 69 6E 67 20 74 6F 20 64 6F 20 73 6F 6D 65 74 68 69 6E 67 2C 20 61 73 20 69 6E 20 61 20 73 75 72 70 72 69 73 65
