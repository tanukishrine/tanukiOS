# tanuki OS

A small 16-bit Forth operating system for the x86.

![main](https://i.imgur.com/veeE0bz.png)


## About

tanuki OS is a small bare-metal operating system designed from
the ground up with the philosophy of extreme software minimalism
and the zeitgeist of Forth programming culture in mind. Its
architecture is driven by these ideas:

- Simplicity: The design rewards highly factored, transparent
  code. With only a small set of powerful abstractions, one is
  able to build complete systems.

- Mindfulness: The restricted nature of the 8086 processor and
  the Forth environment enforces thoughtful use of resources
  available at a hardware level.

- Power: In software, usually what looks simple on the surface
  hides extreme complexity on the inside. Conversely, what looks
  daunting from the outside can reveal itself to be elegantly
  simple on the inside (or more horrendous out of control layers
  of complexity upon complexity). This OS aims to keep itself as
  transparent as possible, allowing the user to take full,
  direct control of their system.

- Extensibility: The system should be able to readily expand to
  meet the needs of the user across a wide variety of
  applications.

The Forth language gives the user all the tools necessary to
abstract away from hardware and, with enough time, produce code
under nearly any programming paradigm.

Alas, due to these goals, the OS is only suitable for the "power
users" who are already familiar with Forth or willing to learn
it. From an end-product perspective, tanuki OS is capable of
producing DOS-like programs. For example, a fork of this
project, [Borbular](https://github.com/tanukishrine/borbular),
demonstrates its graphical capabilities.

The OS also has bootstrapping capabilities, allowing the user
to enter more complex environments.


## Features

- Bootloader: Runs on real hardware and can boot from a floppy
  disk or a USB stick.

- Forth microkernel: A minimal Forth core that is capable of
  expanding itself at runtime.

- Interactive REPL: Provides direct user input and evaluation.

- Block storage: 32 disk blocks for persistent storage of
  programs and text between sessions.

- Line-based text editor: Simple yet capable editor for
  modifying content within block storage.

- 8086 assembler: Can be inlined within Forth words to optimize
  code or access hardware-specific operations.

- Disk I/O: Read and write access to floppy disks.


## Installation

Ensure you have QEMU and NASM installed on your system.

To build and boot from an image, simply run:

```make```

By default, the Makefile will build a floppy image and launch
QEMU to interpret the raw binaries as a 5.25" 360kB floppy.

If you want to test the USB-compatible image, run:

```make usb```

To write the USB image directly to a physical USB drive and 
boot from it, the process should look something like this:

1. Insert the USB drive and identify its device path (for
   example `/dev/sdb`).

2. Unmount any mounted partitions on the USB drive.

3. Write the image:
   `sudo dd if=hdd.img of=/dev/sdb bs=512`

4. Flush pending writes:
   `sync`

5. Eject and remove the USB-drive.


## Design

At the core, the kernel is a "dictionary" comprised of "words"
stringed together as a linked list. When a word is read from the
input stream, either from the user input or block memory, the
interpreter searches backwards (from the most recently defined
word) for a matching word in the dictionary. If the word is
found, its "definition" is executed. Otherwise a "<name> word
not found" error is returned and the program flow is aborted.

The structure of a word laid out as such:

```
[name]           x bytes
[link]           2 bytes
[length + flag]  1 byte
[payload]        x bytes
```

Where `name` is the name of a word, `link` points the previous
dictionary entry, `length + flag` holds the length of the name
and a flag to indicate the "immediateness" of a word (whether
the word get compiled or immediately executed during compilation
mode).

For example, `dup` is defined in this manner:

```
db 'dup'      ; name
dw prev_addr  ; link
db 3          ; length + flag
mov ax, [bp]  ; payload
add bp, 2
mov [bp], ax
ret
```

After the boot process, the instruction immediately jumps to the
`interpret` word which loops back upon itself and waits for user
input. See `kernel.asm` under this respository for more a more
detailed view on how each Forth words are defined.

## Video Demo

[![thumbnail](https://i.imgur.com/riUOrQt.png)](https://i.imgur.com/iUDwRDy.mp4)
