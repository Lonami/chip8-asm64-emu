.intel_syntax noprefix


#; We're using g++ to compile which expects comments to be #
#; but intel syntax highlight uses ; for comments, hence #;.
#;
#; See https://ftp.gnu.org/old-gnu/Manuals/gas-2.9.1/html_chapter/as_7.html
#; for all possible assembler directives.
.data
	#; Some constants for our program
	.set PX_MULT, 16
	.set BITS_PER_PX, 0
	.set VIDEO_FLAGS, 0

	.set SCREEN_FILL, 0xffffff
	.set SCREEN_CLEAR, 0x000000

	#; CHIP-8 had a 4KB memory
	.set PROGRAM_SIZE, 4*1024

	#; The interpreter had the firt 512 bytes reserved,
	#; and most programs were loaded immediatly after.
	.set PROGRAM_START, 0x200

	#; 48 bytes allow 24 levels of function nesting (only use of the stack).
	.set STACK_SIZE, 48

	#; 16 registers, named from V0...VF are available.
	.set REG_COUNT, 16

	#; The processor runs at 60Hz (60 times in a second).
	.set FRAME_DELAY, 1000/60

	#; Some constants defined in SDL headers
	.set SDL_KEYDOWN, 2
	.set SDL_KEYUP, 3
	.set SDL_QUIT, 12

	.set SDL_INIT_AUDIO, 16
	.set SDL_INIT_VIDEO, 32

	.set AUDIO_U8, 8

	.comm screen, 8, 8   #; SDL_Surface* screen
	.comm event, 24, 16  #; SDL_Event event
	.comm rect, 8, 8     #; SDL_Rect

	#; The lowest part of the memory belongs to our interpreter.
	#; We can place the representation of numbers between 0..F here.
	program:
		.byte 0xF0, 0x90, 0x90, 0x90, 0xF0  #; 0
		.byte 0x20, 0x60, 0x20, 0x20, 0x70  #; 1
		.byte 0xF0, 0x10, 0xF0, 0x80, 0xF0  #; 2
		.byte 0xF0, 0x10, 0xF0, 0x10, 0xF0  #; 3
		.byte 0x90, 0x90, 0xF0, 0x10, 0x10  #; 4
		.byte 0xF0, 0x80, 0xF0, 0x10, 0xF0  #; 5
		.byte 0xF0, 0x80, 0xF0, 0x90, 0xF0  #; 6
		.byte 0xF0, 0x10, 0x20, 0x40, 0x40  #; 7
		.byte 0xF0, 0x90, 0xF0, 0x90, 0xF0  #; 8
		.byte 0xF0, 0x90, 0xF0, 0x10, 0xF0  #; 9
		.byte 0xF0, 0x90, 0xF0, 0x90, 0x90  #; A
		.byte 0xE0, 0x90, 0xE0, 0x90, 0xE0  #; B
		.byte 0xF0, 0x80, 0x80, 0x80, 0xF0  #; C
		.byte 0xE0, 0x90, 0x90, 0x90, 0xE0  #; D
		.byte 0xF0, 0x80, 0xF0, 0x80, 0xF0  #; E
		.byte 0xF0, 0x80, 0xF0, 0x80, 0x80  #; F
		.zero PROGRAM_SIZE - 0x50

	#; We also keep track of an intermeiate buffer where bits
	#; represent whether to draw something or not. For this
	#; reason we only need (64*32)/8 -> 256 bytes.
	screenbuffer: .zero 0x100
	#; And some extra padding for an imaginary row, just in case.
	.zero 8

	#; Define the stack and the registers of our machine. These could
	#; potentially live in the reserved memory for the interpreter, as
	#; we don't really use that memory for anything else.
	.comm program_stack, STACK_SIZE
	program_regs: .zero REG_COUNT
	program_regi: .zero 2
	program_delay_timer: .zero 1
	program_sound_timer: .zero 1

	#; Keep 2 bytes (16 bits) to indicate which keys are pressed.
	keys: .zero 2

	#; File Pointer, when loading the ROM.
	.comm fp, 8, 8

	#; Holds the error, if any, to print when exiting.
	err: .space 8

	#; Sound structs
	desiredSpec: .zero 32
	obtainedSpec: .zero 32

	#; Temporary variable for local stuff
	tmp: .space 8

.section .rodata
	#; Some Read Only data such as error messages, title, etc.
	title: .string "CHIP-8"
	readmode: .string "rb"

	err_wrongargs: .string "Wrong argument count (must pass file to load)\n"
	err_openfail: .string "Failed to open file (does it exist?)\n"
	err_initfail: .string "Failed to initialize devices\n"

.text
	.global main


#; SDL has a thread that will run our custom callback whenever the sound
#; buffer needs more data. For something as simple as a beep, we can use
#; a saw wave which only increments while the sound register is non-zero
#; just like if it were a boolean flag.
#;
#; void callback(void *user_data, Uint8 *stream, int length);
audiocallback:
	test byte ptr program_sound_timer[rip], 0xff
	jz ac_done
	#; al holds the saw wave, which goes up to 255 and then overflows to 0.
	xor al, 0
	mov rdi, rsi
	mov rcx, rdx
ac_fillbuffer:
	stosb
	inc al
	loop ac_fillbuffer
ac_done:
	ret


#; Assumes event is set.
#; ah <- Action, or 0 on invalid key
#; al <- Mapped key pressed, if any
#; This method is meant to be called after getkey/getkeylock.
savekey:
	mov ah, byte ptr event[rip]
	cmp ah, SDL_KEYDOWN
	je sk_process
	cmp ah, SDL_KEYUP
	je sk_process
	mov ax, 0
	jmp sk_exit
sk_process:
	mov edx, event[rip+8]
	mov al, dl
	#; Numpad conveniently goes from 0 to 9, check that first
	cmp al, 10
	jl sk_done
	#; Map the key in al to the right one, if applicable. Otherwise, no event.
	cmp al, 'Z'
	jle sk_isupper
	sub al, 'a' - 'A'
sk_isupper:
	#; 1 2 3 4  ->  1 2 3 C
	cmp al, '1'
	je sk_key1
	cmp al, '2'
	je sk_key2
	cmp al, '3'
	je sk_key3
	cmp al, '4'
	je sk_keyc
	#; Q W E R  ->  4 5 6 D
	cmp al, 'Q'
	je sk_key4
	cmp al, 'W'
	je sk_key5
	cmp al, 'E'
	je sk_key6
	cmp al, 'R'
	je sk_keyd
	#; A S D F  ->  7 8 9 E
	cmp al, 'A'
	je sk_key7
	cmp al, 'S'
	je sk_key8
	cmp al, 'D'
	je sk_key9
	cmp al, 'F'
	je sk_keye
	#; Z X C V  ->  A 0 B F
	cmp al, 'Z'
	je sk_keya
	cmp al, 'X'
	je sk_key0
	cmp al, 'C'
	je sk_keyb
	cmp al, 'V'
	je sk_keyf
	#; No valid key pressed, consider no event at all.
	mov ax, 0
	jmp sk_exit
sk_key0:
	mov al, 0x0
	jmp sk_done
sk_key1:
	mov al, 0x1
	jmp sk_done
sk_key2:
	mov al, 0x2
	jmp sk_done
sk_key3:
	mov al, 0x3
	jmp sk_done
sk_key4:
	mov al, 0x4
	jmp sk_done
sk_key5:
	mov al, 0x5
	jmp sk_done
sk_key6:
	mov al, 0x6
	jmp sk_done
sk_key7:
	mov al, 0x7
	jmp sk_done
sk_key8:
	mov al, 0x8
	jmp sk_done
sk_key9:
	mov al, 0x9
	jmp sk_done
sk_keya:
	mov al, 0xa
	jmp sk_done
sk_keyb:
	mov al, 0xb
	jmp sk_done
sk_keyc:
	mov al, 0xc
	jmp sk_done
sk_keyd:
	mov al, 0xd
	jmp sk_done
sk_keye:
	mov al, 0xe
	jmp sk_done
sk_keyf:
	mov al, 0xf
	jmp sk_done
sk_done:
	#; Update the keys bitset
	mov cl, al
	mov dx, 1
	shl dx, cl
	cmp ah, SDL_KEYUP
	je sk_up
	or keys[rip], dx
	jmp sk_exit
sk_up:
	not dx
	and keys[rip], dx
sk_exit:
	ret


#; Draws the screen buffer
drawbuffer:
	push rbx  #; Counter 0 -> 32
	push r12  #; Counter 64 -> 0
	push r15  #; What we'll be drawing
	mov word ptr rect[rip+4], PX_MULT  #; Width
	mov word ptr rect[rip+6], PX_MULT  #; Height
	#; Each of the 32 rows fits in a 64 bit register.
	#; That is, we iterate 32 times reading 64 bits (width).
	xor rbx, rbx
	#; y = 0
	mov word ptr rect[rip+2], 0
	db_loop:
		lea rsi, screenbuffer[rip]
		mov r15, [rsi+rbx*8]
		mov r12, 64
		#; x = 63
		mov word ptr rect[rip+0], 63*PX_MULT
		db_shiftloop:
			mov edx, SCREEN_CLEAR
			mov ecx, SCREEN_FILL
			shr r15
			cmovc edx, ecx  #; there's carry if we shifted a set bit
			mov rdi, screen[rip]
			lea rsi, rect[rip]
			call SDL_FillRect@PLT
			#; x -= 1
			sub word ptr rect[rip+0], PX_MULT
			dec r12
			jnz db_shiftloop
		#; y += 1
		add word ptr rect[rip+2], PX_MULT
		inc rbx
		cmp rbx, 32
		jne db_loop
	#; Update the whole screen.
	mov rdi, screen[rip]
	mov esi, 0
	mov edx, 0
	mov ecx, 0
	mov r8d, 0
	call SDL_UpdateRect@PLT
	pop r15
	pop r12
	pop rbx
	ret


emulateprogram:
	push rbx  #; Program counter
	push r12  #; Stack pointer
	push r13  #; Register base
	mov rbx, PROGRAM_START
	lea r12, program_stack[rip]
	lea r13, program_regs[rip]
ep_loop:
	#; Sleep a bit to emulate CHIP-8's processor speed
	mov edi, FRAME_DELAY
	call SDL_Delay@PLT

ep_checkdelay:
	#; Decrement the delay and sound timer, if they're non-zero
	test byte ptr program_delay_timer[rip], 0xff
	jz ep_checksound
	dec byte ptr program_delay_timer[rip]
ep_checksound:
	test byte ptr program_sound_timer[rip], 0xff
	jz ep_pollevent
	dec byte ptr program_sound_timer[rip]

ep_pollevent:
	#; Poll a key event if any, and exit if requested
	lea rdi, event[rip]
	call SDL_PollEvent@PLT
	test eax, eax
	jz ep_parseop
	cmp byte ptr event[rip], SDL_QUIT
	je ep_quit
	call savekey
ep_parseop:
	lea rsi, program[rip]
	movzx rax, word ptr [rsi+rbx]
	xchg ah, al  #; CHIP-8 endianess is big, while most processors are little
	add rbx, 2
	#; Keep operation to jump in rcx, and mask it away from rax for easier use
	xor rcx, rcx
	mov cl, ah
	shr cl, 4
	and ah, 0x0f
	lea rdx, ep_jumptable[rip]  #; Base of jump table doesn't change
	movsx rcx, dword ptr [rdx+rcx*4]  #; Offset value
	add rcx, rdx  #; Add base to the offset value
	jmp rcx  #; Jump to it
	.section	.rodata
	.align 4
ep_jumptable:
	.long ep_op0 - ep_jumptable
	.long ep_op1 - ep_jumptable
	.long ep_op2 - ep_jumptable
	.long ep_op3 - ep_jumptable
	.long ep_op4 - ep_jumptable
	.long ep_op5 - ep_jumptable
	.long ep_op6 - ep_jumptable
	.long ep_op7 - ep_jumptable
	.long ep_op8 - ep_jumptable
	.long ep_op9 - ep_jumptable
	.long ep_opa - ep_jumptable
	.long ep_opb - ep_jumptable
	.long ep_opc - ep_jumptable
	.long ep_opd - ep_jumptable
	.long ep_ope - ep_jumptable
	.long ep_opf - ep_jumptable
	.text

ep_op0:
	cmp al, 0xe0
	je ep_op0e0
	cmp al, 0xee
	jne ep_loop
	ep_op0ee:
		#; 00:E0 - RET. Return from a subroutine.
		mov bx, [r12]
		sub r12, 2
		jmp ep_loop
	ep_op0e0:
		#; 00:E0 - CLS. Clear the display.
		lea rdi, screenbuffer[rip]
		xor rax, rax
		mov rcx, 32
		rep stosq
		call drawbuffer
		jmp ep_loop
ep_op1:
	#; 1n:nn - JP addr. Jump to location nnn.
	mov bx, ax
	jmp ep_loop
ep_op2:
	#; 2n:nn - CALL addr. Call subroutine at nnn.
	add r12, 2
	mov [r12], bx
	mov bx, ax
	jmp ep_loop
ep_op3:
	#; 3x:kk - SE Vx, byte. Skip next instruction if Vx = kk.
	xor rcx, rcx
	mov cl, ah
	mov dl, [r13+rcx]
	cmp dl, al
	jne ep_loop
	add rbx, 2
	jmp ep_loop
ep_op4:
	#; 4x:kk - SNE Vx, byte. Skip next instruction if Vx != kk.
	xor rcx, rcx
	mov cl, ah
	mov dl, [r13+rcx]
	cmp dl, al
	je ep_loop
	add rbx, 2
	jmp ep_loop
ep_op5:
	#; 5x:y0 - SE Vx, Vy. Skip next instruction if Vx = Vy.
	xor rcx, rcx
	mov cl, ah
	mov dl, [r13+rcx]
	mov cl, al
	shr cl, 4
	cmp dl, [r13+rcx]
	jne ep_loop
	add rbx, 2
	jmp ep_loop
ep_op6:
	#; 6x:kk - LD Vx, byte. Set Vx = kk.
	xor rcx, rcx
	mov cl, ah
	mov [r13+rcx], al
	jmp ep_loop
ep_op7:
	#; 7x:kk - ADD Vx, byte. Set Vx = Vx + kk.
	xor rcx, rcx
	mov cl, ah
	add [r13+rcx], al
	jmp ep_loop
ep_op8:
	#; 8x:y?. Operate with Vx and Vy, may modify VF.
	xor r8, r8
	xor r9, r9
	xor rcx, rcx
	mov cl, ah
	mov r8b, [r13+rcx]
	xor rdx, rdx
	mov dl, al
	shr dl, 4
	cmp r9b, [r13+rdx]

	#; Keep operation to jump in r10
	xor r10, r10
	mov r10w, ax  #; r10b <- ah
	shr r10w, 8+4
	lea rax, ep_jt8[rip]  #; Base of jump table doesn't change
	movsx r10, dword ptr [rax+r10*4]  #; Offset value
	add r10, rax  #; Add base to the offset value
	jmp r10  #; Jump to it
	.section	.rodata
	.align 4
ep_jt8:
	.long ep_op80 - ep_jt8
	.long ep_op81 - ep_jt8
	.long ep_op82 - ep_jt8
	.long ep_op83 - ep_jt8
	.long ep_op84 - ep_jt8
	.long ep_op85 - ep_jt8
	.long ep_op86 - ep_jt8
	.long ep_op87 - ep_jt8
	.long ep_loop - ep_jt8
	.long ep_loop - ep_jt8
	.long ep_loop - ep_jt8
	.long ep_loop - ep_jt8
	.long ep_loop - ep_jt8
	.long ep_loop - ep_jt8
	.long ep_op8e - ep_jt8
	.long ep_loop - ep_jt8
	.text
	ep_op80:
		#; 8x:y0 - LD Vx, Vy. Set Vx = Vy.
		mov [r13+rcx], r9b
		jmp ep_loop
	ep_op81:
		#; 8x:y1 - OR Vx, Vy. Set Vx = Vx OR Vy.
		or [r13+rcx], r9b
		jmp ep_loop
	ep_op82:
		#; 8x:y2 - AND Vx, Vy. Set Vx = Vx AND Vy.
		and [r13+rcx], r9b
		jmp ep_loop
	ep_op83:
		#; 8x:y3 - XOR Vx, Vy. Set Vx = Vx XOR Vy.
		xor [r13+rcx], r9b
		jmp ep_loop
	ep_op84:
		#; 8x:y4 - ADD Vx, Vy. Set Vx = Vx + Vy, set VF = carry.
		add [r13+rcx], r9b
		setc byte ptr 0xf[r13]
		jmp ep_loop
	ep_op85:
		#; 8x:y5 - SUB Vx, Vy. Set Vx = Vx - Vy, set VF = NOT borrow.
		sub [r13+rcx], r9b
		setnc byte ptr 0xf[r13]
		jmp ep_loop
	ep_op86:
		#; 8x:y6 - SHR Vx {, Vy}. Set Vx = Vx SHR 1, set VF = bit shifted out.
		shr r9b
		setc byte ptr 0xf[r13]
		mov [r13+rcx], r9b
		mov [r13+rdx], r9b
		jmp ep_loop
	ep_op87:
		#; 8x:y7 - SUBN Vx, Vy. Set Vx = Vy - Vx, set VF = NOT borrow.
		sub [r13+rdx], r8b
		setnc byte ptr 0xf[r13]
		jmp ep_loop
	ep_op8e:
		#; 8x:yE - SHL Vx {, Vy}. Set Vx = Vx SHL 1, set VF = bit shifted out.
		shl r9b
		setc byte ptr 0xf[r13]
		mov [r13+rcx], r9b
		mov [r13+rdx], r9b
		jmp ep_loop

ep_op9:
	#; 9x:y0 - SNE Vx, Vy. Skip next instruction if Vx != Vy.
	xor rcx, rcx
	mov cl, ah
	mov dl, [r13+rcx]
	mov cl, al
	shr cl, 4
	cmp dl, [r13+rcx]
	je ep_loop
	add rbx, 2
	jmp ep_loop
ep_opa:
	#; An:nn - LD I, addr. Set I = nnn.
	mov program_regi[rip], ax
	jmp ep_loop
ep_opb:
	#; Bn:nn - JP V0, addr. Jump to location nnn + V0.
	xor rcx, rcx
	mov cl, 0[r13]
	add ax, cx
	mov bx, ax
	jmp ep_loop
ep_opc:
	#; Cx:kk - RND Vx, byte. Set Vx = random byte AND kk.
	push rax
	call rand@PLT
	mov rdx, rax
	pop rax
	and dl, al
	xor rcx, rcx
	mov cl, ah
	mov [r13+rcx], dl
	jmp ep_loop
ep_opd:
	#; Dx:yn - DRW Vx, Vy, nibble. Display n-byte sprite starting at memory
	#;                             location I at (Vx, Vy), set VF = collision.
	#;
	#; TODO VF = 1 if any bit erased
	#;      out of bounds wraps to the other side of the screen wrong
	#;
	#; r8b, r9b = Vx, Vy
	xor rcx, rcx
	mov cl, ah
	mov r8b, [r13+rcx]
	mov cl, al
	shr cl, 4
	mov r9b, [r13+rcx]
	#; set up rsi -> program[reg_i]
	lea rsi, program[rip]
	movzx rdx, word ptr program_regi[rip]
	add rsi, rdx
	#; now position rdi -> screenbuffer[y * 8]
	lea rdi, screenbuffer[rip]
	movzx rdx, r9b
	shl rdx, 3
	add rdi, rdx
	#; ch = n
	mov ch, al
	and ch, 0x0f
	#; cl = x
	mov cl, r8b
ep_opdrawloop:
	#; Since screenbuffer works with bits, we'll load data into al
	#; then rotate 8 so it starts at offset 0, then rotate until x.
	xor rax, rax
	lodsb
	#; Rotate so the data is at "position" 0 in the register
	ror rax, 8
	#; Then rotate by the extra "x" to actually position it
	ror rax, cl
	xor [rdi], rax
	add rdi, 8
	dec ch
	jnz ep_opdrawloop
	#; Draw the new buffer
	call drawbuffer
	jmp ep_loop

ep_ope:
	#; Ex:??. Skip on key.
	xor rcx, rcx
	mov cl, ah
	mov cl, [r13+rcx]
	mov dx, 1
	shl dx, cl
ep_opecheck:
	cmp al, 0x9e
	je ep_opex9e
	cmp al, 0xa1
	jne ep_loop
ep_opexa1:
	#; Ex:9E - SKP Vx. Skip next instruction if key[Vx] is pressed.
	test keys[rip], dx
	jz ep_loop
	add rbx, 2
	jmp ep_loop
ep_opex9e:
	#; Ex:A1 - SKNP Vx. Skip next instruction if key[Vx] is not pressed.
	test keys[rip], dx
	jnz ep_loop
	add rbx, 2
	jmp ep_loop

ep_opf:
	#; Fx:??. Special registers (I, delay, sound, key) and misc.
	xor rcx, rcx
	mov cl, ah  #; [r13+rcx] = register
	mov r8b, al
	shr r8b, 4
	mov r9b, al
	and r9b, 0x0f
	cmp r8b, 0
	je ep_opf0
	cmp r8b, 1
	je ep_opf1
	cmp r8b, 2
	je ep_opf2
	cmp r8b, 3
	je ep_opf3
	cmp r8b, 5
	je ep_opf5
	cmp r8b, 6
	jne ep_loop
	ep_opf6:
		#; Fx:65 - LD Vx, [I]. Read registers V0 through Vx from memory
		#;                     starting at location I.
		cmp r9b, 0x05
		jne ep_loop
		movzx rax, word ptr program_regi[rip]
		lea rsi, program[rip]
		add rsi, rax
		lea rdi, program_regs[rip]
		add word ptr program_regi[rip], cx
		rep movsb
		jmp ep_loop

	ep_opf0:
		cmp r9b, 0x07
		je ep_opf07
		cmp r9b, 0x0a
		jne ep_loop
		ep_opf0a:
			#; Fx:0A - LD Vx, K. Vx = wait for key press.
			#; Do NOT push rcx to the stack. WaitEvent breaks.
			mov tmp[rip], rcx
		ep_opf0a_getkey:
			lea rdi, event[rip]
			call SDL_WaitEvent@PLT
			#; Returns 0 on error while waiting for events
			test eax, eax
			jz ep_quit
			cmp byte ptr event[rip], SDL_QUIT
			je ep_quit
			call savekey
			test ah, ah
			jz ep_opf0a_getkey
			mov rcx, tmp[rip]
			mov [r13+rcx], al
			jmp ep_loop
		ep_opf07:
			#; Fx:07 - LD Vx, DT. Set Vx = delay timer value.
			mov al, program_delay_timer[rip]
			mov [r13+rcx], al
			jmp ep_loop
	ep_opf1:
		movzx ax, byte ptr [r13+rcx]
		cmp r9b, 0x05
		je ep_opf15
		cmp r9b, 0x08
		je ep_opf18
		cmp r9b, 0x0e
		jne ep_loop
		ep_opf1e:
			#; Fx:1E - ADD I, Vx. Set I = I + Vx.
			add program_regi[rip], ax
			cmp word ptr program_regi[rip], 0xfff
			seta byte ptr 0xf[r13]  #; set VF if bound overflow (above)
			jmp ep_loop
		ep_opf15:
			#; Fx:15 - LD DT, Vx. Set delay timer = Vx.
			mov program_delay_timer[rip], al
			jmp ep_loop
		ep_opf18:
			#; Fx:18 - LD ST, Vx. Set sound timer = Vx.
			mov program_sound_timer[rip], al
			jmp ep_loop
	ep_opf2:
		#; Fx:29 - LD F, Vx. Set I = location of sprite for digit Vx.
		cmp r9b, 0x09
		jne ep_loop
		movzx rax, byte ptr [r13+rcx]
		lea rax, [rax+rax*4]
		mov word ptr program_regi[rip], ax
		jmp ep_loop
	ep_opf3:
		#; Fx:33 - LD B, Vx. Store BCD representation of Vx in memory
		#;                  locations I, I+1, and I+2.
		cmp r9b, 0x03
		jne ep_loop
		movzx ax, byte ptr [r13+rcx]
		xor dx, dx
		mov cx, 100
		div cx
		mov program_regi[rip+0], al
		mov ax, dx
		xor dx, dx
		mov cx, 10
		div cx
		mov program_regi[rip+1], al
		mov program_regi[rip+2], dl
		jmp ep_loop
	ep_opf5:
		#; Fx:55 - LD [I], Vx. Store registers V0 through Vx in memory
		#;                    starting at location I.
		cmp r9b, 0x05
		jne ep_loop
		lea rsi, program_regs[rip]
		movzx rax, word ptr program_regi[rip]
		lea rdi, program[rip]
		add rdi, rax
		add word ptr program_regi[rip], cx
		rep movsb
		jmp ep_loop
ep_quit:
	pop r13
	pop r12
	pop rbx
	ret


main:
	#; SDL expects us to have an activation frame, where rbp -> rsp.
	push rbp
	mov rbp, rsp

	#; First we check that we have enough arguments (ROM to load)
	lea rdx, err_wrongargs[rip]
	mov err[rip], rdx
	cmp rdi, 2
	jne error

	#; If we do, pick the pointer to the 2nd string argument and load the ROM
	mov rdi, 8[rsi]
	lea rsi, readmode[rip]
	call fopen@PLT
	lea rdx, err_openfail[rip]
	mov err[rip], rdx
	test rax, rax
	jz error

	mov fp[rip], rax
	lea rdi, program+PROGRAM_START[rip]
	mov rsi, PROGRAM_SIZE
	mov rdx, 1
	mov rcx, rax
	call fread@PLT

	#; Initialize the RNG so we can call rand()
	mov edi, 0
	xor eax, eax
	call time@PLT
	mov edi, eax
	call srand@PLT

	#; We'll be using video and audio, so enable both
	mov edi, SDL_INIT_VIDEO
	or edi, SDL_INIT_AUDIO
	call SDL_Init@PLT
	lea rdx, err_initfail[rip]
	mov err[rip], rdx
	test rax, rax
	jnz error

	#; Set the window title to something relevant
	lea rsi, title[rip]
	lea rdi, title[rip]
	call SDL_WM_SetCaption@PLT

	#; Prepare the video mode, so we actually have the dimensions on screen.
	#; We have a constant PX_MULT and instead drawing single pixels and then
	#; letting SDL resize them to a larger window size (which we should figure
	#; out how to do at some point), we draw rectangles of dimensions PX_MULT.
	mov edi, 64*PX_MULT
	mov esi, 32*PX_MULT
	mov edx, BITS_PER_PX
	mov ecx, VIDEO_FLAGS
	call SDL_SetVideoMode@PLT
	test rax, rax
	jz quit
	mov screen[rip], rax

	#; Prepare the audio device (this is legacy, but really simple as well).
	lea rsi, obtainedSpec[rip]
	lea rdi, desiredSpec[rip]
	mov dword ptr desiredSpec[rip], 44100      #; spec.freq = 44100
	mov word ptr desiredSpec[rip+4], AUDIO_U8  #; spec.format = AUDIO_U8
	mov byte ptr desiredSpec[rip+6], 1         #; spec.channels = 1
	mov word ptr desiredSpec[rip+8], 2048      #; spec.samples = 2048
	lea rax, audiocallback[rip]
	mov QWORD PTR desiredSpec[rip+16], rax     #; spec.callback = callback
	call SDL_OpenAudio@PLT
	cmp rax, 0
	jl quit

	#; Start the audio (set pause = 0)
	xor edi, edi
	call SDL_PauseAudio@PLT

	#; Everything ready, enter the routine that will emulate the read program
	call emulateprogram
quit:
	call SDL_Quit@PLT
	jmp exit

error:
	#; On error, err will point to the string containing the error message
	mov rdi, err[rip]
	#; printf accepts floating point variable arguments, but we are
	#; not using any. We tell it so by xor'ing away the rax register.
	xor rax, rax
	call printf@PLT
	mov eax, 1
	jmp .exit

exit:
	mov eax, 0
.exit:
	#; Leave the activation frame, and return the main function exiting
	leave
	ret
