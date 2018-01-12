.intel_syntax noprefix

.data
	.set PX_MULT, 16
	.set BITS_PER_PX, 0
	.set VIDEO_FLAGS, 0

	.set SDL_KEYDOWN, 2
	.set SDL_KEYUP, 3
	.set SDL_QUIT, 12
	.set SDL_INIT_VIDEO, 32

	#; comm name, size, alignment
	.comm screen, 8, 8   #; SDL_Surface* screen
	.comm event, 24, 16  #; SDL_Event event
	.comm rect, 8, 8     #; SDL_Rect

	.set SCREEN_FILL, 0xffffff
	.set SCREEN_CLEAR, 0x000000

	.set PROGRAM_SIZE, 0x1000
	.set PROGRAM_START, 0x200
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

	screenbuffer: .zero 0x100  #; (64*32)/8 -> 256
	.zero 8  #; extra padding, wrap around not emulated perfectly yet...

	#; file pointer
	.comm fp, 8, 8

	.set STACK_SIZE, 48
	.comm program_stack, STACK_SIZE

	.set REG_COUNT, 16
	program_regs: .zero REG_COUNT
	program_regi: .zero 2
	program_delay_timer: .zero 1
	program_sound_timer: .zero 1

	#; 16 keys, as bits
	keys: .zero 2
	#; last key pressed, as integer
	lastkey: .byte

	err: .space 8

	.set FRAME_DELAY, 1000/60

.section .rodata
	title: .string "CHIP-8"
	readmode: .string "rb"

	err_wrongargs: .string "wrong argument count (must pass file to load)\n"
	err_openfail: .string "failed to open file (does it exist?)\n"

.text
	.global main


#; ah -> event
#; al -> key pressed
#; ah <- 0 on invalid key
#; al <- mapped key pressed, if any
#; meant to be called after getkey/getkeylock
savekey:
	cmp ah, SDL_KEYDOWN
	je sk_process
	cmp ah, SDL_KEYUP
	je sk_process
	mov ax, 0
	jmp sk_exit
sk_process:
	mov edx, event[rip+8]
	mov al, dl
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
	#; no valid key pressed, consider no event
	mov ah, 0
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
	mov lastkey[rip], al
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


#; ah = event (SDL_QUIT/SDL_KEYDOWN)
#; al = mapped key pressed, if any
getkeylock:
	push rbp
	mov rbp, rsp
	lea rdi, event[rip]
	call SDL_WaitEvent@PLT
	jmp gk_process
getkey:
	#; Important: SDL_PollEvent expects rbp = rsp (?) or bad things happen
	push rbp
	mov rbp, rsp
	lea rdi, event[rip]
	call SDL_PollEvent@PLT
gk_process:
	test eax, eax
	jz gk_exit
	#; First check if event is quit
	mov ah, byte ptr event[rip]
	cmp ah, SDL_QUIT
	je gk_exit
	#; Then if it was keydown
	call savekey
gk_exit:
	leave
	ret


#; draws the screen buffer
drawbuffer:
	push rbx  #; counter 0 -> 32
	push r12  #; counter 64 -> 0
	push r15  #; what we'll be drawing
	mov word ptr rect[rip+4], PX_MULT  #; width
	mov word ptr rect[rip+6], PX_MULT  #; height
	#; each of the 32 rows fits in a 64 bit register
	#; that is we iterate 32 times reading 64 bits (width)
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
	#; update everything
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
	push rbx  #; program counter
	push r12  #; stack pointer
	push r13  #; register base
	push r14  #; preserved but temporary
	push r15  #; preserved but temporary
	mov rbx, PROGRAM_START
	lea r12, program_stack[rip]
	lea r13, program_regs[rip]
ep_loop:
	mov edi, FRAME_DELAY
	call SDL_Delay@PLT
	call getkey@PLT
	cmp ah, SDL_QUIT
	je ep_quit
	xor r8, r8  #; r8 holds the key pressed if any
	mov r8b, al
ep_parseop:
	lea rsi, program[rip]
	movzx rax, word ptr [rsi+rbx]
	xchg ah, al  #; endianness
	add rbx, 2
	#; keep op to jump in rcx, mask it away from rax
	xor rcx, rcx
	mov cl, ah
	shr cl, 4
	and ah, 0x0f
	lea rdx, ep_jumptable[rip]  #; base of jump table doesn't change
	movsx rcx, dword ptr [rdx+rcx*4]  #; offset value
	add rcx, rdx  #; add base to the offset value
	mov rdx, r8  #; key pressed in rdx
	jmp rcx  #; jump to it
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

	#; keep op to jump in r10, mask it away from rax
	xor r10, r10
	mov r10w, ax  #; r10b <- ah
	shr r10w, 8+4
	lea rax, ep_jt8[rip]  #; base of jump table doesn't change
	movsx r10, dword ptr [rax+r10*4]  #; offset value
	add r10, rax  #; add base to the offset value
	jmp r10  #; jump to it
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
	#;                            location I at (Vx, Vy), set VF = collision.
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
	#; since screenbuffer works with bits, we'll load data into al
	#; then rotate 8 so it starts at offset 0, then rotate until x
	xor rax, rax
	lodsb
	#; rotate so the data is at "position" 0 in the register
	ror rax, 8
	#; then rotate by the extra "x" to actually position it
	ror rax, cl
	xor [rdi], rax
	add rdi, 8
	dec ch
	jnz ep_opdrawloop
	#; draw the new buffer
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
			push rcx
		ep_opf0a_getkey:
			call getkeylock
			cmp ah, SDL_QUIT
			je ep_quit
			cmp ah, SDL_KEYDOWN
			jne ep_opf0a_getkey
			pop rcx
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
	pop r15
	pop r14
	pop r13
	pop r12
	pop rbx
	ret


main:
	push rbp
	mov rbp, rsp

	#; check we have enough arguments
	lea rdx, err_wrongargs[rip]
	mov err[rip], rdx
	cmp rdi, 2
	jne error

	#; we do so pick the pointer to the 2nd string
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

	#; init rng
	mov edi, 0
	xor eax, eax
	call time@PLT
	mov edi, eax
	call srand@PLT

	mov edi, SDL_INIT_VIDEO
	call SDL_Init@PLT

	lea rsi, title[rip]
	lea rdi, title[rip]
	call SDL_WM_SetCaption@PLT

	mov edi, 64*PX_MULT
	mov esi, 32*PX_MULT
	mov edx, BITS_PER_PX
	mov ecx, VIDEO_FLAGS
	call SDL_SetVideoMode@PLT
	mov screen[rip], rax

	call emulateprogram
	call SDL_Quit@PLT
	jmp exit

error:
	mov rdi, err[rip]
	xor rax, rax
	call printf@PLT
	mov eax, 1
	jmp .exit

exit:
	mov eax, 0
.exit:
	leave
	ret
