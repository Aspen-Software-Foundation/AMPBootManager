#!/bin/bash
set -e

nasm -f bin -o bin/boot.ase bl.asm
cp bin/boot.ase bin/bootloader.ase
cp bin/boot.ase bin/bootloader_floppy.ase

if [[ "$1" == "-iso" ]]; then
  truncate -s 2048 bin/bootloader.ase
  mkdir -p iso/boot
  cp bin/bootloader.ase iso/bootloader.ase
  xorriso -as mkisofs -b bootloader.ase -no-emul-boot -boot-load-size 1 -o os.iso iso/
  qemu-system-x86_64 -drive file=os.iso,media=cdrom,if=ide,format=raw -boot d

elif [[ "$1" == "-fdd" ]]; then
  truncate -s 1474560 bin/bootloader_floppy.ase
  qemu-system-x86_64 -fda bin/bootloader_floppy.ase -boot a

else
  echo "Usage: $0 -iso | -fdd"
  exit 1
fi