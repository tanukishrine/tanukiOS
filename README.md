### tanuki OS

A small 16-bit Forth operating system for the x86.

![main](https://i.imgur.com/veeE0bz.png)


#### About

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
enter more complex environments.


#### Features

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


#### Running the OS

Ensure you have QEMU and NASM installed on your system.

To build and boot from an image, simply run:

```make```

By default, Makefile will build a floppy image and launch QEMU
to interpret the raw binaries as a 5.25" 360kB floppy.

If you desire to test the USB compatible image, run:

```make usb```

To load the USB image directly to a physical USB drive and 
boot from it, the process should look something like this:

1. Insert the USB drive and identify its device path (for
   example `/dev/sdb`).

2. Unmount any mounted partitions on the USB drive.

3. Write the image:
   `sudo dd if=hdd.img of=/dev/sdb bs=512`

4. Flush pending writes:
   `sync`

5. Eject and remove the USB drive.
