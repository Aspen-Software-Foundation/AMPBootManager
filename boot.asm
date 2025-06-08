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
    jmp .bldd_common

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
    mov cx, 5       ; Counter
    mov si, dap     ; SI gets offset
    mov bx, si      ; BX = offset of DAP (within DS)
    
.reset:
    mov dl, [BOOT_DRIVE]
	mov ah, 0	; reset disk
	int 0x13
	jc .checkreset	; reset again if error

    mov ah, 0x42                 ; Extended read
    mov dl, [BOOT_DRIVE]
    int 0x13
    jc .checkreset
    jmp .no_error             ; jump to loaded kernel

    mov si, badcode_error_string_1

.errcommon:
    mov [ERRDATA], ah

    call print_si
    mov si, msg_common_2
    call print_si

    mov dl, [ERRDATA]
    call print_hex_dl
    jmp halt_b16rm

.checkreset:
    mov si, badcode_error_string_3
    dec cx
    test cx, cx
    jz .errcommon
    jmp .reset


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

badcode_error_string_1 db 13, 10, 'ERR: 0x0BCE... ', 0
badcode_error_string_2 db 13, 10, 'ERR: 0x1BCE', 0
badcode_error_string_3 db 13, 10, 'ERR: 0x1CBD', 0
msg_bootneardone db 13, 10, 'NBT', 0

align 16
dap:
    db 16                   ; size of DAP (16 bytes) 
    db 0                    ; reserved (1 byte)
    dw SECTOR_COUNT_READOP  ; Operation size (like in NASM when u move data without saying word or byte, but in sectors and for reading disks)
    dw KERNEL_LOCATION      ; offset (BX)
    dw 0x0000               ; segment (ES)
    dd 1                    ; Lower 32 bits of starting sector
    dd 0                    ; Upper 16 bits of starting sector (almost always 0)



times 446 - ($ - $$) db 0
; MBR PARTITION TABLE HERE!!!!!!!!!!111!!!!
times 510 - ($ - $$) db 0
dw 0xAA55
; NOTE FOR FUTURE
; PLACE GUID PT HERE

jmp bootdone

    
rdap0:
    db 16                   ; size of DAP (16 bytes) EXCLUDING drive id
    db 0                    ; reserved (1 byte)
    dw SECTOR_COUNT_READOP  ; Operation size (like the NASM err when u move data between two addressses without saying word or byte, but in sectors and for reading disks)
    dw 0xA000               ; offset (BX)
    dw 0x0000               ; segment (ES)
    dd 16                   ; Lower 32 bits of starting sector
    dd 0                    ; Upper 32 bits of starting sector (almost always 0)
    dd 0                    ; DRIVE NUMBER



SECTORS_PER_TRACK:
    dw 0x12 ; idk maybe there would be more than 255 or some shi
    
HEADS_PER_CYLINDER:
    dw 0x02 ; idk maybe there would be more than 255 or some shi

; convert LBA addressing to CHS addressing
; these outputs can be fed directly into int 0x13, AH 0x02
; based on this StackOverflow answer:
; https://stackoverflow.com/questions/45434899/why-isnt-my-root-directory-being-loaded-fat12/45495410#45495410
; sector:   (LBA mod SPT) + 1
; head:     (LBA / SPT) mod heads
; cylinder: (LBA / SPT) / heads
; inputs:
; 	SI: LBA
; outputs:
; 	DH: head
; 	CH: cylinder
; 	CL: sector/cylinder
lba2chs:
    push ax
    mov ax, si
    xor dx, dx
    div word [SECTORS_PER_TRACK] ; LBA / SPT
    mov cl, dl
    inc cl                        ; CL = (LBA mod SPT) + 1
    xor dx, dx
    div word [HEADS_PER_CYLINDER] ; (LBA / SPT) / heads
    mov dh, dl                    ; DH = (LBA / SPT) mod heads
    mov ch, al                    ; CH = (LBA / SPT) / heads
    shl ah, 6                     ; store upper 2 bits of 10-bit cylinder into...
    or cl, ah                     ; ...upper 2 bits of cector (CL)
    pop ax
    ret


; Read Disk
; inputs:
; 	SI: Pointer to Drive Numbered Disk Address Packet (DNDAP)
;
; DNDAP Structure:
;    Offset 0x00, Size 1, Exp: size of DAP (16 bytes) EXCLUDING drive id (DN)
;    Offset 0x01, Size 1, Exp: reserved
;    Offset 0x02, Size 2, Exp: Operation size (So how many Sectors to read/write/execute/.....)
;    Offset 0x04, Size 2, Exp: offset (BX)
;    Offset 0x06, Size 2, Exp: segment (ES)
;    Offset 0x08, Size 4, Exp: Lower 32 bits of starting sector
;    Offset 0x0C, Size 4, Exp: Upper 32 bits of starting sector (almost always 0)
;    Offset 0x10, Size 1, Exp: DRIVE NUMBER
DiskRead16bRM:
    mov di, .dap            ; DI points to the destination
    mov cx, 17              ; Number of bytes to copy
    rep movsb               ; Copy `CX` bytes from `[SI]` to `[DI]`

    mov ah, 0x41            ; Check if LBA is supported
    mov bx, 0x55AA          ; Magic number required by BIOS
    int 0x13                ; BIOS disk function

    jc .use_CHS             ; If carry flag is set, LBA is not supported
    jmp .use_LBA            ; If LBA is supported, jump to LBA routine

.use_CHS:                       ; NOTE: I AM USING A LOCAL HERE BCUZ PERHAPS I SHOULD ADD LBA IN THE FUTURE?
    mov si, [.dap + 2]
    call lba2chs

    mov bx, [.dap + 6]          ; read address
    shl bx, 1
    add bx, [.dap + 4]

    mov ah, 0x02                ; BIOS CHS disk read function
    mov al, [.dap + 0x08]       ; Number of sectors to read
    mov dl, [.dap + 0x10]       ; Disk number
    
    int 0x13                    ; BIOS disk
    jnc .done

    mov si, badcode_error_string_1
    jmp .errcommon
    
.use_LBA:
    mov cl, 5       ; Counter
    mov si, .dap
    mov bx, si      ; BX = offset of DAP (within DS)
    
.reset:
    mov dl, [.dap + 0x10]
	mov ah, 0	; reset disk
	int 0x13
	jc .checkreset	; reset again if error

    mov ah, 0x42                 ; Extended read
    mov dl, [.dap + 0x10]
    int 0x13
    jc .checkreset
    jmp .done             ; jump to loaded 
.checkreset:
    dec cl
    test cl, cl
    jz .errcommon
    jmp .reset
    
.errcommon:
    stc

.done:
    ret

align 16
.dap:
    db 16                   ; size of DAP (16 bytes) EXCLUDING drive id
    db 0                    ; reserved (1 byte)
    dw SECTOR_COUNT_READOP  ; Operation size (like the NASM err when u move data between two addressses without saying word or byte, but in sectors and for reading disks)
    dw 0xA000               ; offset (BX)
    dw 0x0000               ; segment (ES)
    dd 16                   ; Lower 32 bits of starting sector
    dd 0                    ; Upper 32 bits of starting sector (almost always 0)
    dd 0                    ; DRIVE NUMBER

dd 0 ; saefti


msg_bootdone db 'Welcome to the ', 0
db 'AMPBootManager', 0
db ', made for the ', 0
db 'Aspen Multi-Platform Operating System', 0
db '!', 0

HALT_PM_B32:
    cli
    hlt
    jmp HALT_PM_B32

bootdone:
    mov ah, 0x00               ; BIOS set video mode function
    mov al, 0x03               ; Mode 3 = 80x25 text mode
    int 0x10                   ; BIOS video interrupt
    
    mov dl, [BOOT_DRIVE]
    cmp dl, 0x80
    jb .s512
    cmp dl, 0xE0
    jb .s512
    cmp dl, 0xF0
    jb .s2048

    jmp .s512               ; Other Device. Please fix in future

.s2048:
    mov word  [rdap0+0x02], 1
    mov dword [rdap0+0x08], 16
    mov [rdap0+0x10], dl
    jmp .readop

.s512:
    mov word  [rdap0+0x02], 4
    mov dword [rdap0+0x08], 67
    mov [rdap0+0x10], dl
    
.readop:
    mov si, rdap0
    call DiskRead16bRM

    mov ebx, 0xB8000
    mov ecx, 0

.loop0:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0F
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop0
    
    inc ecx
.loop1:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0B
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop1
    
    inc ecx
.loop2:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0F
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop2
    
    inc ecx
.loop3:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0C
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop3
    
    inc ecx
.loop4:
    mov al, [msg_bootdone + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0F
    add ebx, 2
    inc ecx
    cmp byte [msg_bootdone + ecx], 0
    jne .loop4

    mov ecx, 0
    mov edx, 7
.done:
    mov al, [0xA000 + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0F
    add ebx, 2
    inc ecx
    dec edx
    cmp edx, 0
    jne .done

    jmp HALT_PM_B32


times SECTOR_COUNT_ISO_PAD_BYTES - ($ - $$) db 0


; 69 20 6D 65 61 6E 74 20 69 20 77 61 73 20 67 6F 69 6E 67 20 74 6F 20 64 6F 20 73 6F 6D 65 74 68 69 6E 67 2C 20 61 73 20 69 6E 20 61 20 73 75 72 70 72 69 73 65
