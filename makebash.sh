#!/bin/bash
set -e

# make
mkdir -p bin
nasm -f bin -o bin/boot.ase boot.asm
nasm -f elf32 -o bin/stageloading.o entry16.asm
nasm -f elf32 -o bin/kernel_entry.o entry32.asm
gcc -m32 -o bin/kmain.o -c kmain.c -nostdlib -ffreestanding -nostartfiles -fno-pie -fno-pic -Werror -Wall  
# -Wall basically places a wall in ur terminal. -Werror brings Wario and Waluigi into the error detection process
# i use GCC cuz maybe some ppl dont have gcc-elf-iX86 (HINT: Me, i don't have time)

ld -m elf_i386 -T linker.ld bin/stageloading.o bin/kernel_entry.o bin/kmain.o -o bin/kernel.ase


# ISO ination
cp bin/boot.ase bin/bootloader.ase
cp bin/boot.ase bin/bootloader_floppy.ase

truncate -s 1474560 bin/bootloader_floppy.ase
truncate -s 327680  bin/bootloader.ase


mkdir -p iso/boot
cp bin/bootloader.ase iso/bootloader.ase
cp bin/kernel.ase    iso/kernel.ase
xorriso -as mkisofs -iso-level 3 -eltorito-boot bootloader.ase -no-emul-boot -boot-load-size 4\
 -isohybrid-gpt-basdat -o os.iso iso/
# -eltorito-alt-boot -e EFI/BOOT/BOOTX64.EFI -no-emul-boot
cp os.iso og.iso

dd if=os.iso of=gpt_primary.bin bs=512 skip=1 count=33
sectors=$(($(stat -c%s os.iso)/512))
dd if=os.iso of=gpt_backup.bin bs=512 skip=$((sectors-33)) count=33


dd if=os.iso bs=16384 skip=2 count=2 of=isofs.bin status=none conv=notrunc

dd if=isofs.bin bs=16384 seek=2 of=bin/bootloader.ase        status=none conv=notrunc
dd if=isofs.bin bs=16384 seek=2 of=bin/bootloader_floppy.ase status=none conv=notrunc

dd if=bin/bootloader_floppy.ase bs=2048 skip=0 count=16 of=bootsect.bin status=none conv=notrunc
dd if=bootsect.bin              bs=2048 skip=0          of=os.iso       status=none conv=notrunc

dd if=gpt_primary.bin of=os.iso bs=512 seek=1               status=none conv=notrunc
dd if=gpt_backup.bin  of=os.iso bs=512 seek=$((sectors-33)) status=none conv=notrunc

cp os.iso fos.iso
truncate -s 1474560 fos.iso


if [[ "$1" == "-iso" ]]; then
  qemu-system-x86_64 -drive file=os.iso,media=cdrom,if=ide,format=raw -boot d

elif [[ "$1" == "-fdd" ]]; then
  qemu-system-x86_64 -drive format=raw,if=floppy,file=fos.iso -boot a
  
elif [[ "$1" == "-hdd" ]]; then
  qemu-system-x86_64 -drive format=raw,if=ide,file=os.iso -boot c

else
  echo "Usage: $0 -iso | -fdd | -hdd"
  exit 1
fi
