[org 0x7C00]
[bits 16]
KERNEL_LOCATION equ 0x7C00    ; Due to some bugs i present to you...... Reloading the loaded sectors
KERNEL_JUMPADDR equ 0xC400    ; Where kernel will be loaded (0x7c00 + 512*36)

SECTOR_COUNT_MINIMUM equ 0x40         ; Number of sectors to read

SECTOR_COUNT_ISO_PAD equ (SECTOR_COUNT_MINIMUM + 3) / 4
SECTOR_COUNT_READOP equ (SECTOR_COUNT_ISO_PAD*4)

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
    mov dl, [BOOT_DRIVE]
    mov [0x7000], dl
    jmp readsectorsATTEMPT

.cdrom:
    mov si, dev_cdrom
    call comncmn

    mov ax, [dap + 2]
    shr ax, 2
    mov [dap + 2], ax
    
    mov dl, [BOOT_DRIVE]
    mov [0x7000], dl

    jmp readsectorsATTEMPT

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
    mov cl, 0x01                ; Sector 5 (first sector is 1)
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
    mov dl, [0x7000]
    mov [BOOT_DRIVE], dl
    mov si, msg_bootneardone
    call print_si

jmp KERNEL_JUMPADDR

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
badcode_error_string_3 db 13, 10, 'ERR: 0x1CBD', 0
msg_bootneardone db 13, 10, 'NBT', 0

align 16
dap:
    db 16                   ; size of DAP (16 bytes) 
    db 0                    ; reserved (1 byte)
    dw SECTOR_COUNT_READOP  ; Operation size (like in NASM when u move data without saying word or byte, but in sectors and for reading disks)
    dw KERNEL_LOCATION      ; offset (BX)
    dw 0x0000               ; segment (ES)
    dd 0                    ; Lower 32 bits of starting sector
    dd 0                    ; Upper 16 bits of starting sector (almost always 0)


times 446 - ($ - $$) db 0
; MBR PARTITION TABLE HERE!!!!!!!!!!111!!!!
times 510 - ($ - $$) db 0
dw 0xAA55

; NOTE FOR FUTURE
; PLACE GUID PT HERE
; PS: USE A PARTITIONING TOOL IN MAKEBASH.SH FOR THIS

times (512*36) - ($ - $$) db 0

jmp bootdone

    
rdap0:
    db 16                   ; size of DAP (16 bytes) EXCLUDING drive id
    db 0                    ; reserved (1 byte)
    dw SECTOR_COUNT_READOP  ; Operation size (like the NASM err when u move data between two addressses without saying word or byte, but in sectors and for reading disks)
    dw 0x8000               ; offset (BX)
    dw 0x0000               ; segment (ES)
    dd 16                   ; Lower 32 bits of starting sector
    dd 0                    ; Upper 32 bits of starting sector (almost always 0)
    db 0                    ; DRIVE NUMBER




dd 0 ; saefti

print_hex_dl2: 
    push eax

    mov al, dl 
    shr al, 4 
    call .nib 
    mov al, dl 
    and al, 0x0F 
    call .nib 
     
    pop eax
    ret 
.nib: 
    add al, '0' 
    cmp al, '9' 
    jbe .store 
    add al, 7 ; Convert A-F 
.store: 
    mov [ebx], al ; Write character at EBX 
    mov byte [ebx+1], 0x0F ; Set attribute (white text) 
    add ebx, 2 ; Move to next character position 
    ret







; DEBUG ONLY SHT
test_lba2chs:
    ; Input: SI = LBA sector number
    ; Output: CH = Cylinder, CL = Sector, DH = Head (Used for INT 0x13, AH=0x02)
    mov si, 22
    call lba2chs  ; Convert LBA to CHS

    ; Debug print: Check if values are correct
    mov dl, ch
    call print_hex_dl2  ; Print Cylinder
    mov dl, cl
    call print_hex_dl2  ; Print Sector
    mov dl, dh
    call print_hex_dl2  ; Print Head

    ret


dbsdbg:
    push eax
    push ebx
    mov ebx, 0xB8000

    call print_hex_dl2
    mov dl, dh
    call print_hex_dl2

    mov dl, cl
    call print_hex_dl2
    mov dl, ch
    call print_hex_dl2

    mov eax, ebx
    pop ebx
    mov dl, bl
    mov dh, bh
    mov ebx, eax
    call print_hex_dl2
    mov dl, dh
    call print_hex_dl2

    pop eax
    
    mov dl, al
    call print_hex_dl2
    mov dl, ah
    call print_hex_dl2


    cli
    hlt
; OK END OF DEBUG FNs



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

ESPLACE:
    dw 0

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
;    Offset 0x11, Size 1, Exp: reserved
DiskRead16bRM:
    mov di, .dap            ; DI points to the destination
    mov cx, 18              ; Number of bytes to copy
    rep movsb               ; Copy `CX` bytes from `[SI]` to `[DI]`

    mov ah, 0x41            ; Check if LBA is supported
    mov bx, 0x55AA          ; Magic number required by BIOS
    int 0x13                ; BIOS disk function

    jc .use_CHS             ; If carry flag is set, LBA is not supported
    jmp .use_LBA            ; If LBA is supported, jump to LBA routine
    
    mov bx, es
    mov [ESPLACE], bx

.use_CHS:
    mov bx, [.dap + 4]      ; Offset (BX)

    mov ah, 0x02            ; INT 13h read
    mov al, [.dap + 2]      ; Sector count (1 byte here, safe)

    mov si, [.dap + 8]      ; Starting LBA (you pass lower 16 bits only?)
    call lba2chs            ; You better fill CH, CL, DH inside

    mov dl, [.dap + 0x10]   ; Custom disk # field — NOT standard, but ok
    
    mov bx, [.dap + 6]      ; Segment (ES)
    mov es, bx
    mov bx, [.dap + 4]      ; THIS MADE ME EAT GRASS
    int 0x13                ; Fire BIOS read
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
    mov si, badcode_error_string_2

    dec cl
    test cl, cl
    jz .errcommon
    jmp .reset
    
.errcommon:
    mov bx, [ESPLACE]
    mov es, bx

    call print_si
    stc

.done:
    mov bx, [ESPLACE]
    mov es, bx

    ret

align 16
.dap:
    db 16                   ; size of DAP (16 bytes) EXCLUDING drive id
    db 0                    ; reserved (1 byte)
    dw SECTOR_COUNT_READOP  ; Operation size (like the NASM err when u move data between two addressses without saying word or byte, but in sectors and for reading disks)
    dw 0x8000               ; offset (BX)
    dw 0x0000               ; segment (ES)
    dd 16                   ; Lower 32 bits of starting sector
    dd 0                    ; Upper 32 bits of starting sector (almost always 0)
    dw 0                    ; DRIVE NUMBER

SHIFT_FACTOR: dd 0

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
    mov byte [SHIFT_FACTOR], 0
    jmp .readop

.s512:
    mov word  [rdap0+0x02], 4
    mov dword [rdap0+0x08], 64
    mov [rdap0+0x10], dl
    mov byte [SHIFT_FACTOR], 2
    
.readop:
    mov si, rdap0
    mov ebx, 0xB8000
    mov ecx, 0

    call DiskRead16bRM
    jc .OffsetPrerun

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
    mov [TEMP_REG32_3], ebx
    jmp DiskParser
.OffsetPrerun:
    mov ebx, 0xB8110
    mov ecx, 0
    jmp .loop0

DiskParser:
    ; time to process the BS we printed
    mov si, CORRECT_CD001
    mov di, 0x8000
    mov cx, 6
    repe cmpsb   ; Black magic which translates to "Compare bytes" in 8086 speak
    jz .Continue

.NotEqual:
    mov ecx, 0
.NotEqual0:
    mov al, [Matchnt + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0F
    add ebx, 2
    inc ecx
    cmp byte [Matchnt + ecx], 0
    jne .NotEqual0

    mov ecx, 0
    mov edx, 6
.NotEqual1:
    mov al, [0x8000 + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0F
    add ebx, 2
    inc ecx
    dec edx
    cmp edx, 0
    jne .NotEqual1
    
    jmp HALT_PM_B32   

.Continue:
    mov ecx, 0
.Continue0:
    mov al, [Matchs + ecx]
    mov [ebx], al
    mov byte [ebx + 1], 0x0F
    add ebx, 2
    inc ecx
    cmp byte [Matchs + ecx], 0
    jne .Continue0
    mov [TEMP_REG32_3], ebx

    mov dl, [BOOT_DRIVE]

    cmp dl, 0x80
    jb .t512
    cmp dl, 0xE0
    jb .t512
    cmp dl, 0xF0
    jb .t2048

    jmp .t512               ; Other Device. Please fix in future

.t2048:
    mov eax, [0x80A6]
    
    add eax, 2047      ; add divisor - 1 to round up
    shr eax, 11        ; divide by 2048
    
    mov [ROOT_PVD_SECTOR_COUNT], eax
    mov [rdap0+0x02], ax

    mov eax, [0x809E]
    mov [rdap0+0x08], eax
    jmp .treadop

.t512:
    mov eax, [0x80A6]
    
    add eax, 2047      ; add divisor - 1 to round up
    shr eax, 11        ; divide by 2048

    mov [ROOT_PVD_SECTOR_COUNT], eax
    shl eax, 2
    mov [rdap0+0x02], ax

    mov eax, [0x809E]
    shl eax, 2
    mov [rdap0+0x08], eax    

.treadop:
    clc
    mov [rdap0 + 0x10], dl

    mov si, rdap0
    call DiskRead16bRM
    mov ecx, 0

.parseloop:
    mov ah, [0x8000 + ecx]
    test ah, ah
    jnz .valid

    ; zero.... NEXT SECTOR (if available)
    mov [TEMP_REG32_0], ecx
    mov [TEMP_REG32_1], eax

    jmp DiskParser.NotEqual


;    ; Basically we do `ECX//(2<<11)` to get mod
;    shr ecx, 11
;    shl ecx, 11
;
;    ; now we have what to sub by to get mod
;    mov eax, ecx
;    mov ecx, [TEMP_REG32_0]
;
;    sub ecx, eax
;
;    ; EDIT: :facepalm: i coulda just `and ecx, (2<<11)-1`
    and ecx, (2<<11)-1
    test ecx, ecx
    jz DiskParser.NotEqual ; we are on the first gah damn byte of the secta so we musta F'ed up

    mov ecx, [TEMP_REG32_0] ; restore counter
    add ecx, 2047      ; add divisor - 1 to round up
    shr ecx, 11        ; divide by 2048 (aka 2<<11). ECX now contains what sector it ***SHOULD*** jump to

    mov ax, [ROOT_PVD_SECTOR_COUNT] ; store into AX the size of the root PVD in sectors
    test cx, ax  ; see if we hit the end
    je DiskParser.NotEqual ; Error 404: file not found

.valid:
    mov [TEMP_REG32_0], ecx
    mov ah, [ecx + 0x8020] ; 0x8020 = 0x8000 + 32 (32 is offset for filename length)
    mov [NAME_LEN], ah
    
    mov ah, [ecx + 0x8000] ; entry size is at sart of etnry
    mov [ENTRY_LEN], ah

    mov eax, ecx
    add eax, 0x8021
    mov di, ax
    
    mov eax, BOOTFILE
    mov si, ax
    
    mov ax, [NAME_LEN]

    mov ebx, BOOTFILE
    mov si, bx

    call strlen
    
;    push ecx
;    push ebx
;    mov dl, bl
;    mov ebx, [TEMP_REG32_3]
;    mov ecx, [TEMP_REG32_0]
;    call print_hex_dl2
;    mov dl, al
;    call print_hex_dl2
;    pop ebx
;    pop ecx

    cmp al, bl
    jne .continee ; Naw its too short
    ; hol up maybe this the right file?

    mov eax, BOOTFILE
    mov si, ax
    
    mov eax, ecx
    add eax, 0x8021
    mov di, ax

    mov cx, [NAME_LEN]

    repe cmpsb
    jnz .continee
    ; CHAT THIS THE ONE
    ; Assuming `entry` is a memory location
    ; Load entry_lba from entry[2:6] (4 bytes, little-endian)
    
    mov dl, [BOOT_DRIVE]

    cmp dl, 0x80
    jb .u512
    cmp dl, 0xE0
    jb .u512
    cmp dl, 0xF0
    jb .u2048

    jmp .t512               ; Other Device. Please fix in future

.u2048:
    mov ecx, [TEMP_REG32_0]
    mov eax, [0x800A + ecx]
    
    add eax, 2047      ; add divisor - 1 to round up
    shr eax, 11        ; divide by 2048
    mov [rdap0+0x02], ax

    mov eax, [0x8002 + ecx]
    mov [rdap0+0x08], eax
    jmp .ureadop

.u512:
    mov ecx, [TEMP_REG32_0]
    mov eax, [0x800A + ecx]
    
    add eax, 2047      ; add divisor - 1 to round up
    shr eax, 11        ; divide by 2048
    shl eax, 2         ; shr then shl to mask. might be faster to use an and operation but maybe later
    mov [rdap0+0x02], ax

    mov eax, [0x8002 + ecx]
    shl eax, 2
    mov [rdap0+0x08], eax

.ureadop:
    clc
    mov word [rdap0 + 4], 0xF000
    mov word [rdap0 + 6], 0x0000
    mov [rdap0 + 0x10], dl

    mov si, rdap0
    call DiskRead16bRM

    call clear_screen

    mov ax, 0xF000
    mov si, ax
    mov ax, 0x0000
    mov es, ax

    mov dl, [BOOT_DRIVE]
    mov ebx, 0xB8000
    jmp si
    jmp HALT_PM_B32

.continee:    
    mov ecx, [TEMP_REG32_0]
    mov dl, ch
    call print_hex_dl2
    mov dl, cl
    call print_hex_dl2
    
    mov byte [ebx], ' '
    add ebx, 2

    mov [TEMP_REG32_3], ebx

    mov eax, [ENTRY_LEN]
    add ecx, eax

    test eax, eax
    jz .parseloop

    jmp .valid


    jmp HALT_PM_B32


CORRECT_CD001:
    db 0x01, "CD001"

ROOT_PVD_SECTOR_COUNT:
    dd 0
    
NAME_LEN:
    dd 0

ENTRY_LEN:
    dd 0

BOOTFILE:
    dd "KERNEL.ASE;1", 0

TEMP_REG32_0:
    dd 0
TEMP_REG32_1:
    dd 0
TEMP_REG32_2:
    dd 0
TEMP_REG32_3:
    dd 0

Matchnt:
    db " ~~ Bruh, signature not match wtf... It says, and I quote:", 0
Matchs:
    db " ~~ IT WURKZ!!!! WE GOT PVD BOYS LETS FKN GOOOU", 0


; strlen: DS:SI = ptr to null-terminated string
; returns AX = length

strlen:
    mov bx, 0
.strl01:
    cmp byte [si + bx], 0
    je .strend 
    inc bx
    jmp .strl01
.strend:
    ret

SWAP_BUFFER:
    dd 0

clear_screen:
    push es
    push di
    push ax
    push cx

    mov ax, 0xB000
    mov es, ax
    mov di, 0x8000      ; Point DI to video memory
    mov ax, 0x0720      ; 'Space' character (0x20) with attribute (0x07 → black on white)
    mov cx, 2000        ; 80x25 screen = 2000 characters

    rep stosw           ; Fill the screen with spaces (clears everything)
    
    pop cx
    pop ax
    pop di
    pop es
    ret


times SECTOR_COUNT_ISO_PAD_BYTES - ($ - $$) db 0


; 69 20 6D 65 61 6E 74 20 69 20 77 61 73 20 67 6F 69 6E 67 20 74 6F 20 64 6F 20 73 6F 6D 65 74 68 69 6E 67 2C 20 61 73 20 69 6E 20 61 20 73 75 72 70 72 69 73 65
