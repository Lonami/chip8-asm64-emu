.intel_syntax noprefix

.data
	.set PX_MULT, 16
	.set SCREEN_WIDTH, 64*PX_MULT
	.set SCREEN_HEIGHT, 32*PX_MULT
	.set BITS_PER_PX, 0
	.set VIDEO_FLAGS, 0

	.set SDL_QUIT, 12
	.set SDL_INIT_VIDEO, 32

	#; comm name, size, alignment
	.comm screen, 8, 8   #; SDL_Surface* screen
	.comm event, 24, 16  #; SDL_Event event
	.comm rect, 8, 8     #; SDL_Rect
	gameover: .byte 0    #; loop until game over

	.set PROGRAM_SIZE, 0x1000
	.comm program, PROGRAM_SIZE, 32
	.comm fp, 8, 8

	err: .space 8

.section .rodata
	title: .string "CHIP-8"
	readmode: .string "rb"

	err_wrongargs: .string "wrong argument count (must pass file to load)\n"
	err_openfail: .string "failed to open file (does it exist?)\n"

.text
	.global main

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
	lea rdi, program[rip]
	mov rsi, PROGRAM_SIZE
	mov rdx, 1
	mov rcx, rax
	call fread@PLT

	mov edi, SDL_INIT_VIDEO
	call SDL_Init@PLT

	lea rsi, title[rip]
	lea rdi, title[rip]
	call SDL_WM_SetCaption@PLT

	mov edi, SCREEN_WIDTH
	mov esi, SCREEN_HEIGHT
	mov edx, BITS_PER_PX
	mov ecx, VIDEO_FLAGS
	call SDL_SetVideoMode@PLT
	mov screen[rip], rax

	mov word ptr rect[rip], 8*PX_MULT
	mov word ptr rect[rip+2], 4*PX_MULT
	mov word ptr rect[rip+4], 48*PX_MULT
	mov word ptr rect[rip+6], 24*PX_MULT

renderloop:
	lea rdi, event[rip]
	call SDL_PollEvent@PLT
	test eax, eax
	je noevent

	mov al, byte ptr event[rip]
	cmp al, SDL_QUIT
	sete al
	mov gameover[rip], al
noevent:
	mov rdi, screen[rip]
	mov esi, 0  #; no rect
	mov edx, 0x000000
	call SDL_FillRect@PLT

	mov rdi, screen[rip]
	lea rsi, rect[rip]
	mov edx, 0xff0000
	call SDL_FillRect@PLT

	mov rdi, screen[rip]
	mov esi, 0
	mov edx, 0
	mov ecx, 0
	mov r8d, 0
	call SDL_UpdateRect@PLT
.L2:
	test byte ptr gameover[rip], 1
	jz renderloop

quit:
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
