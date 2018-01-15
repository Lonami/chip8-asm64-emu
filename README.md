CHIP-8 64-bits assembly emulator
================================

This emulator was written from scratch originally with the help alone of
the [Wikipedia (CHIP-8)](https://en.wikipedia.org/wiki/CHIP-8), and later
on with [Cowgod's Manual](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM).

The emulator is written in a single `main.s` assembly file and graphics are
rendered to screen through the help of [SDL](https://www.libsdl.org/), which
you need if you want to compile. The emulator was written for the x86-64
platform under Linux (important to note that the data is stored in memory
as little-endian, primarily to swap two bytes, as the CHIP-8 uses big-endian).

The file is heavily commented, so you should be able to follow along even
if you have little experience with assembly, and I hope it can teach people
something.

As for a quick overview on the code, the machine registers and program
memory are just "variables" defined on the `.data` segment.

There are methods to set/unset the pressed state of the keys, to draw
the screen buffer, and to beep while the sound timer is not zero.

The `emulateprogram` method reads instructions one by one and parses the
operation by jumping to the right position (e.g. `0xFx29` jumps to `F`),
further performing more comparisons if more fine-tuning is necessary to
determine the operation to execute. There's a delay to mimic the 60Hz of
the machine, and events (e.g. key events) are polled right proceeding.

The `main` method reads the ROM into memory and sets up SDL, exiting on error.

I do not own any of the ROMs available in the `roms/` folder, which were
picked up from other several websites hosting them, neither I do own the
text written by Thomas P. Greene `CHIP8-TECHNICAL.txt`.

An amazing reference on calling conventions (and more) for the x86-64
platform is available at http://wiki.osdev.org/System_V_ABI#x86-64.

You can find most of the used SDL methods on https://wiki.libsdl.org/.
