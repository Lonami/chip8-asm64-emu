.intel_syntax noprefix

.data
	.set PX_MULT, 16
	.set BITS_PER_PX, 0
	.set VIDEO_FLAGS, 0

	.set SDL_KEYDOWN, 2
	.set SDL_QUIT, 12
	.set SDL_INIT_VIDEO, 32

	#; comm name, size, alignment
	.comm screen, 8, 8   #; SDL_Surface* screen
	.comm event, 24, 16  #; SDL_Event event
	.comm rect, 8, 8     #; SDL_Rect

	.set SCREEN_FILL, 0xff0000
	.set SCREEN_CLEAR, 0x00ffff

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

	err: .space 8

.section .rodata
	title: .string "CHIP-8"
	readmode: .string "rb"

	err_wrongargs: .string "wrong argument count (must pass file to load)\n"
	err_openfail: .string "failed to open file (does it exist?)\n"

.text
	.global main



#; draws the screen buffer
drawbuffer:
	push rbx  #; counter 0 -> 32
	push r12  #; counter 64 -> 0
	mov word ptr rect[rip+4], PX_MULT  #; width
	mov word ptr rect[rip+6], PX_MULT  #; height
	#; each of the 32 rows fits in a 64 bit register
	#; that is we iterate 32 times reading 64 bits (width)
	xor rbx, rbx
	#; y = 0
	mov word ptr rect[rip+2], 0
	db_loop:
		lea rsi, screenbuffer[rip]
		mov rax, [rsi+rbx*8]
		mov r12, 64
		#; x = 63
		mov word ptr rect[rip+0], 63*PX_MULT
		db_shiftloop:
			mov edx, SCREEN_CLEAR
			mov ecx, SCREEN_FILL
			shr rax
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
	lea rdi, event[rip]
	call SDL_PollEvent@PLT
	xor r8, r8  #; r8 holds the key pressed if any
	test eax, eax
	jz ep_parseop

	mov al, byte ptr event[rip]
	cmp al, SDL_QUIT
	je ep_quit

	cmp al, SDL_KEYDOWN
	jne ep_parseop
	mov r8d, DWORD PTR event[rip+8]
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
		#; 00:EE -> return
		mov bx, [r12]
		sub r12, 2
		jmp ep_loop
	ep_op0e0:
		#; 00:E0 -> clear display
		lea rdi, screenbuffer[rip]
		xor rax, rax
		mov rcx, 32
		rep stosq
		call drawbuffer
		jmp ep_loop
ep_op1:
	#; 1N:NN -> jump to NNN
	mov bx, ax
	jmp ep_loop
ep_op2:
	#; 2N:NN -> call subroutine at NNN
	add r12, 2
	mov [r12], bx
	mov bx, ax
	jmp ep_loop
ep_op3:
	#; 3X:NN -> skip next instruction if Vx == NN
	xor rcx, rcx
	mov cl, ah
	mov dl, [r13+rcx]
	cmp dl, al
	jne ep_loop
	add rbx, 2
	jmp ep_loop
ep_op4:
	#; 4X:NN -> skip next instruction if Vx != NN
	xor rcx, rcx
	mov cl, ah
	mov dl, [r13+rcx]
	cmp dl, al
	je ep_loop
	add rbx, 2
	jmp ep_loop
ep_op5:
	#; 5X:Y0 -> skip next instruction if Vx == Vy
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
	#; 6X:NN -> set Vx to NN
	xor rcx, rcx
	mov cl, ah
	mov [r13+rcx], al
	jmp ep_loop
ep_op7:
	#; 7X:NN -> Vx += NN, do NOT modify carry flag
	xor rcx, rcx
	mov cl, ah
	add [r13+rcx], al
	jmp ep_loop
ep_op8:
	#; 8X:YZ -> save to Vx after operate with Vy
	#; 		Z = 0 -> MOV
	#; 		Z = 1 -> OR
	#; 		Z = 2 -> AND
	#; 		Z = 3 -> XOR
	#; 		Z = 4 -> ADD (VF = carry)
	#; 		Z = 5 -> SUB (VF = !borrow)
	#;		Z = 6 -> (VF least significant bit); Vx = Vy = (Vy >> 1)
	#;      Z = 7 -> Vy -= Vx, note swapped (VF = !borrow)
	#;		Z = E -> (VF most significant bit); Vx = Vy = (Vy << 1)
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
		mov [r13+rcx], r9b
		jmp ep_loop
	ep_op81:
		or [r13+rcx], r9b
		jmp ep_loop
	ep_op82:
		and [r13+rcx], r9b
		jmp ep_loop
	ep_op83:
		xor [r13+rcx], r9b
		jmp ep_loop
	ep_op84:
		add [r13+rcx], r9b
		setc byte ptr 0xf[r13]  #; set VF if carry
		jmp ep_loop
	ep_op85:
		sub [r13+rcx], r9b
		setnc byte ptr 0xf[r13]  #; set VF if NOT borrow (carry)
		jmp ep_loop
	ep_op86:
		shr r9b
		setc byte ptr 0xf[r13]  #; set VF to the bit shifted out
		mov [r13+rcx], r9b
		mov [r13+rdx], r9b
		jmp ep_loop
	ep_op87:
		sub [r13+rdx], r8b
		setnc byte ptr 0xf[r13]  #; set VF if NOT borrow (carry)
		jmp ep_loop
	ep_op8e:
		shl r9b
		setc byte ptr 0xf[r13]  #; set VF to the bit shifted out
		mov [r13+rcx], r9b
		mov [r13+rdx], r9b
		jmp ep_loop

ep_op9:
	#; 9X:Y0 -> skip next instruction if Vx != Vy
	xor rcx, rcx
	mov cl, ah
	mov dl, [r13+rcx]
	mov cl, al
	shr cl, 8
	cmp dl, [r13+rcx]
	je ep_loop
	add rbx, 2
	jmp ep_loop
ep_opa:
	#; AN:NN -> set reg I to NNN
	mov program_regi[rip], ax
	jmp ep_loop
ep_opb:
	#; BN:NN -> jump to V0+NNN
	xor rcx, rcx
	mov cl, 0[r13]
	add ax, cx
	mov bx, ax
	jmp ep_loop
ep_opc:
	#; CX:NN -> Vx = rand() & NN
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
	#; DX:YN -> draw(coord x = Vx, coord y = Vy, height = N), width = 8
	#; TODO VF = 1 if any bit erased
	#;      out of bounds should wraps to the other side of the screen wrong
	#;
	#; set up rsi -> program[reg_i]
	lea rsi, program[rip]
	movzx rdx, word ptr program_regi[rip]
	add rsi, rdx
	#; now position rdi -> screenbuffer[y * 8]
	lea rdi, screenbuffer[rip]
	movzx rdx, al
	and dl, 0xf0
	shr dl  #; 16 / 2 = 8, required multiplier
	add rdi, rdx
	#; ch = n
	mov ch, al
	and ch, 0x0f
	#; cl = x
	mov cl, ah
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
	#; EX:[9E|A1]
	xor rcx, rcx
	mov cl, ah
	mov cl, [r13+rcx]
	cmp al, 0x9e
	je ep_opex9e
	cmp al, 0xa1
	jne ep_loop
ep_opexa1:
	#; skip next instruction if key() != Vx
	cmp dl, cl
	je ep_loop
	add rbx, 2
	jmp ep_loop
ep_opex9e:
	#; skip next instruction if key() == Vx
	cmp dl, cl
	jne ep_loop
	add rbx, 2
	jmp ep_loop

ep_opf:
	#; FX:IJ
	#;   if I = 0:
	#;     if J = 7: Vx = get_delay()
	#;     if J = A: Vx = get_key() (blocking)
	#;   if I = 1:
	#;     if J = 5: delay_timer(Vx)
	#;     if J = 8: sound_timer(Vx)
	#;     if J = E: I += Vx
	#;   if I = 2, J = 9: I = sprite_addr[Vx], characters in 4x5 font
	#;     if J = 9
	#;   if I = J = 3: set_BCD(Vx), at I -> I hundred, I+1 tens, I+2 digit
	#;   IF I = J = 5: store from V0 to Vx inclusive into I, increasing I
	#;   IF I = 6, J = 5: load to V0 to Vx inclusive from I, increasing I
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
		cmp r9b, 0x05
		jne ep_loop
		#; load registers into V0 to Vx
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
			#;SDL_WaitEvent
			jmp ep_loop
		ep_opf07:
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
			add program_regi[rip], ax
			cmp word ptr program_regi[rip], 0xfff
			seta byte ptr 0xf[r13]  #; set VF if bound overflow (above)
			jmp ep_loop
		ep_opf15:
			mov program_delay_timer[rip], al
			jmp ep_loop
		ep_opf18:
			mov program_sound_timer[rip], al
			jmp ep_loop
	ep_opf2:
		cmp r9b, 0x09
		jne ep_loop
		#; TODO I = sprite_addr[Vx]
		jmp ep_loop
	ep_opf3:
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
		cmp r9b, 0x05
		jne ep_loop
		#; dump registers from V0 to Vx
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
