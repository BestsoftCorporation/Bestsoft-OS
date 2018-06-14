#!/bin/sh



if test "`whoami`" != "root" ; then
	echo "You must be logged in as root to build (for loopback mounting)"
	echo "Enter 'su' or 'sudo bash' to switch to root"
	exit
fi


if [ ! -e BestsoftOS.flp ]
then
	mkdosfs -C BestsoftOS.flp 1440 
fi


nasm -O0 -w+orphan-labels -f bin -o  bootloader.bin Bootloader.asm

nasm -O0 -w+orphan-labels -f bin -o kernel.bin Kernel.asm 

dd status=noxfer conv=notrunc if=bootloader.bin of=BestsoftOS.flp


rm -rf tmp-loop

mkdir tmp-loop && mount -o loop -t vfat BestsoftOS.flp tmp-loop && cp kernel.bin tmp-loop/

sleep 0.2

umount tmp-loop 

rm -rf tmp-loop

rm -f BesotsoftOS.iso
mkisofs -quiet -V 'BESTOS' -input-charset iso8859-1 -o BestsoftOS.iso -b BestsoftOS.flp ./ 


echo '>>> Done!'
