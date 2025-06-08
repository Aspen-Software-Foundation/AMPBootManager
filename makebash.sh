#!/bin/bash
set -e

mkdir -p ./bin

nasm -f bin -o bin/boot.ase boot.asm
cp bin/boot.ase bin/bootloader.ase
cp bin/boot.ase bin/bootloader_floppy.ase

truncate -s 1474560 bin/bootloader_floppy.ase
truncate -s 81920 bin/bootloader.ase

dd if=os.iso bs=16384 skip=2 count=2 of=tmp.bin

dd if=tmp.bin bs=16384 seek=2 of=bin/bootloader.ase status=progress conv=notrunc
dd if=tmp.bin bs=16384 seek=2 of=bin/bootloader_floppy.ase status=progress conv=notrunc


mkdir -p iso/boot
cp bin/bootloader.ase iso/bootloader.ase
xorriso -as mkisofs -b bootloader.ase -no-emul-boot -boot-load-size 4 -o os.iso iso/

if [[ "$1" == "-iso" ]]; then
  qemu-system-x86_64 -drive file=os.iso,media=cdrom,if=ide,format=raw -boot d

elif [[ "$1" == "-fdd" ]]; then
  qemu-system-x86_64 -drive format=raw,if=floppy,file=bin/bootloader_floppy.ase -boot a
  
elif [[ "$1" == "-hdd" ]]; then
  qemu-system-x86_64 -drive format=raw,if=ide,file=bin/bootloader.ase -boot c

else
  echo "Usage: $0 -iso | -fdd | -hdd"
  exit 1
fi
