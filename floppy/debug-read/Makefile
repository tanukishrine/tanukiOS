default: run

fdd_boot.bin: fdd_boot.asm
	nasm $< -o $@

kernel.bin: kernel.asm
	nasm $< -o $@
	truncate -s $$((512 * 17)) $@

blocks.bin: blocks.asm
	nasm $< -o $@
	truncate -s $$((512 * 64)) $@

# ==============================================================
# 1.44MB (3.5") floppy = 2880 sectors
# 360KB (5.25") floppy = 720 sectors
# ==============================================================

floppy.img: fdd_boot.bin kernel.bin blocks.bin
	dd if=/dev/zero of=$@ bs=512 count=720
	dd if=fdd_boot.bin of=$@ bs=512 count=1 conv=notrunc
	dd if=kernel.bin of=$@ bs=512 seek=1 conv=notrunc
	dd if=blocks.bin of=$@ bs=512 seek=18 conv=notrunc

clean:
	rm *.bin
	rm *.img

run: floppy.img
	qemu-system-i386 \
	-drive file=$<,if=floppy,format=raw # -boot a
