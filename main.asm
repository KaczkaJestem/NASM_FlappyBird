; Flappy Bird Game written in x64 NASM
; Target: 64-bit Linux
;
; Author: Adrian Kiełbowicz
;

; ===== EXTERNALS =====

extern printf

; ===== BSS SECTION =====

section		.bss

; ----- Structures -----

; Screen Info struct:
; resx : dw
; resy : dw
; xoffset : dw
; yoffset : dw
; bits_per_pixel : dw
; line_length : dw
;
scr_info			resb	24

; Drawing context struct:
; screen_info_ptr : qw
; framebuffer_ptr : qw
;
drawing_ctx			resb 	16

; Termios struct representation
;
; struct termios {
;     tcflag_t c_iflag;               /* input mode flags */
;     tcflag_t c_oflag;               /* output mode flags */
;     tcflag_t c_cflag;               /* control mode flags */
;     tcflag_t c_lflag;               /* local mode flags */
;     cc_t c_line;                    /* line discipline */
;     cc_t c_cc[NCCS];                /* control characters; NCCS = 19 */
; };
;
; where sizeof(tcflag_) = 4 and sizeof(cc_t) = 1
;
termios				resb	36

; Rectangle struct:
; xpos : dw
; ypos : dw
; width : dw
; height : dw
;
rect_background		resb 	16

; Player struct:
; player_rect : rectangle
; y_pos : dw (float)
; y_velocity : dw (float)
;
player				resb	24

; Physics context struct:
; gravity_acc : dw (float)
; jump_vel : dw (float)
; time_coeff : dw (float)
; acc_tune : dw (float)
;
physics_ctx			resb	16

; Timeval struct representation:
;
; struct timeval {
; 	time_t      tv_sec;     /* seconds */
; 	suseconds_t tv_usec;    /* microseconds */
; };
; 

; ----- Variables -----

framebuffer_size	resb 	8

; ===== READ ONLY DATA SECTION =====

section		.rodata

; ----- Strings -----
str_info_fclose_fail	db	'Failed to close file!', 0xa, 0
str_err_fopen			db	'Failed to open file!', 0xa, 0
str_finfo_err			db	'Error reading fixed screen information', 0xa, 0
str_vinfo_err			db	'Error reading variable screen information', 0xa, 0
str_ioctl_finfo_err		db	'IOCTL syscall failed when reading fixed screen info!', 0xa, 0
str_ioctl_vinfo_err		db	'IOCTL syscall failed when reading variable screen info!', 0xa, 0
str_ioctl_tcgets_err	db	'IOCTL syscall failed when getting terminal parameters!', 0xa, 0
str_ioctl_tcsets_err	db	'IOCTL syscall failed when setting terminal parameters!', 0xa, 0
str_mmap_err			db	'MMAP syscall failed - cannot map framebuffer into memory!', 0xa, 0
str_munmap_err			db	'MUNMAP syscall failed - cannot unmap framebuffer from memory!', 0xa, 0
str_brk_err				db	'BRK syscall failed. Cannot allocate memory!', 0xa, 0
str_gettimeofday_err	db	'GETTIMEOFDAY syscall failed!', 0xa, 0
str_err_code			db	'Error code: %d', 0xa, 0

str_no_blink			db '\033[?12l', 0

str_dbg_f				db '%f', 0xa, 0
str_dbg_d				db '%d', 0xa, 0

file_framebuffer		db	'/dev/fb0', 0

const_player_rect_size	dd	32

; ===== DATA SECTION =====

section		.data

; ----- Variables -----
ticks_since_keypress_space	db	KEYPRESS_COOLDOWN
ticks_since_keypress_q		db	KEYPRESS_COOLDOWN

time_since_last_jump 		dq	JUMP_COOLDOWN

; ----- Pointers -----

; we make them "null pointers" at the beginning
framebuffer_ptr			dq	0
framebuffer_backup_ptr	dq	0
initial_break_ptr		dq	0
current_break_ptr		dq	0

; ----- File descriptors -----
fd_stdin			equ	0
fd_framebuffer		dd	-1

; ----- Pseudo randomness -----
prng_seed		dd	0

; ----- Equate Directives -----
SYS_READ			equ 0
SYS_OPEN    		equ 2
SYS_CLOSE			equ 3
SYS_MMAP			equ 9
SYS_MUNMAP			equ 11
SYS_BRK				equ 12
SYS_IOCTL			equ 16
SYS_EXIT			equ 60
SYS_FCNTL			equ 72
SYS_GETTIMEOFDAY	equ 96
SYS_TIME			equ 201

O_RDONLY    		equ 0
O_WRONLY			equ 1
O_RDWR				equ 2
O_NONBLOCK			equ 2048

F_GETFL				equ 3
F_SETFL				equ 4

EAGAIN				equ -11

FBIOGET_VSCREENINFO	equ 0x4600
FBIOGET_FSCREENINFO	equ 0x4602

FBIOGET_VSCREENINFO_SIZE	equ 160
FBIOGET_FSCREENINFO_SIZE	equ 80

FSCREENINFO_LINE_LENGTH_OFFSET	equ 48

VSCREENINFO_XRES_OFFSET		equ 0
VSCREENINFO_YRES_OFFSET		equ 4
VSCREENINFO_XOFFSET_OFFSET	equ 16
VSCREENINFO_YOFFSET_OFFSET	equ 20
VSCREENINFO_BPP_OFFSET		equ 24

SCRINFO_XRES				equ 0
SCRINFO_YRES				equ 4
SCRINFO_XOFFSET				equ 8
SCRINFO_YOFFSET				equ 12
SCRINFO_BPP					equ 16
SCRINFO_LINE_LEN			equ 20

DRAWINGCTX_SCR_INFO			equ 0
DRAWINGCTX_FRAMEBUFFER_PTR	equ 8

PHYSCTX_GRAVITY_ACC			equ 0
PHYSCTX_JUMP_VEL			equ 4
PHYSCTX_TIME_COEFF			equ 8
PHYSCTX_ACC_TUNE			equ 12

RECT_XPOS					equ 0
RECT_YPOS					equ 4
RECT_WIDTH					equ 8
RECT_HEIGHT					equ 12

PLAYER_RECT					equ 0
PLAYER_Y_POS				equ 16
PLAYER_Y_VEL				equ 20

PROT_READ					equ 1
PROT_WRITE					equ 2

MAP_SHARED					equ 1

MAP_FAILED					equ -1

TCGETS						equ 0x5401
TCSETS						equ 0x5402
ICANON						equ 2
ECHO						equ 8

TIMEVAL_SEC					equ 0
TIMEVAL_USEC				equ 8

ZF_MASK						equ 0x0040
ZF_MASK_SHIFT				equ 6

; ----- Key codes -----
KEY_SPACE	equ	32
KEY_Q		equ 113
KEY_S		equ	115
KEY_W		equ 119

; ----- Colors -----
COLOR_RED		equ 0x00ff0000
COLOR_GREEN		equ 0x0000ff00
COLOR_BLUE		equ 0x000000ff

COLOR_GREY		equ 0x000f0f0f
COLOR_YELLOW	equ 0x00ffff00

; ----- Cooldowns -----
JUMP_COOLDOWN		equ	100000	; microseconds
KEYPRESS_COOLDOWN	equ	8		; ticks

; ===== TEXT SECTION =====

section     .text

global      _start 

; ----- Functions -----

; Prints an info message to the console
; @param rdi - ptr to message string
;
print_info_msg:
	sub rsp, 8
        xor eax, eax    ; printf is varargs, so EAX counts # of non-integer arguments being passed
        call printf
	add rsp, 8
	ret

; Quits the program with a message and return code =-1
; @param rdi - ptr to message string
;
quit_with_error:
	sub rsp, 8
    xor eax, eax    ; printf is varargs, so EAX counts # of non-integer arguments being passed        
	call printf
	add rsp, 8

	mov rdi, -1		; exit code
	jmp exit		; tailcall


; Returns system time in rax - number of seconds since 1970;
; used mainly as seed for prng
; 
get_sys_time:
	; We use system call sys_time,
	; by passing 0 as the param we obtain the result in rax
        mov     rax, SYS_TIME
        xor     rdi, rdi         
        syscall
	ret


; Returns current time in microseconds in rax.
;
get_time_usec:
	; reserve space for timeval struct on the stack
	sub rsp, 16
	; GetTimeOfDay syscall will return timeval struct as a result
	mov rax, SYS_GETTIMEOFDAY
	mov rdi, rsp
	mov rsi, 0
	syscall

	; check for errors
	cmp rax, 0
	je .l_return

	add rsp, 16

	push rax
	mov rdi, str_err_code
	mov rsi, rax
	xor eax, eax
	call printf
	pop rax

	mov rdi, str_gettimeofday_err
	jmp quit_with_error ; tailcall

	.l_return:
		; convert seconds to microseconds
		; by multiplying them by 1,000,000
		mov rax, qword[rsp + TIMEVAL_SEC]
		mov rcx, 15625
		mul rcx							
		shl rax, 6	

		add rax, qword[rsp + TIMEVAL_USEC]
		add rsp, 16
		ret
	

; Returns pseudo random 32-bit integer in eax
; @param edi - lower boundary (inclusive)
; @param esi - upper boundary (exclusive)
; @param rdx - seed ptr
;
get_rand_int:
	; xorshift LFSR with 13, 17, 5 triplet
	mov eax, dword [rdx]
	mov ecx, eax
	shl ecx, 13
	xor eax, ecx
	mov ecx, eax
	shr ecx, 17
	xor eax, ecx
	mov ecx, eax
	shl ecx, 5
	xor eax, ecx
	mov dword [rdx], eax
	
	; clamp the result
	mov ecx, esi
	sub ecx, edi
	; we have to zero edx for the purpose of div:
	xor edx, edx
	div ecx
	mov eax, edx
	add eax, edi
 
	ret


; Opens a file for reading, returns file descriptor in rax
; @param rdi - ptr to filename string 
;
open_file_rdwr:
	mov rax, SYS_OPEN	; sys_open, returns file descriptor in rax
	mov rsi, 2		; O_RDWR
	mov rdx, 0q0666		; umode: can r/w, but not exec
	syscall


	; check if open was successful
	cmp rax, 0
	jl .l_file_open_err
	ret 

	.l_file_open_err:
		; print errno code
		push rax
		mov rdi, str_err_code
		mov rsi, rax
		xor eax, eax
		call printf
		pop rax

		mov edi, str_err_fopen
		jmp quit_with_error	; tailcall


; Closes a file
; @param rdi - file descriptor to close
;
close_file:
	mov eax, SYS_CLOSE
	syscall

	; check if open was successful
	cmp rax, 0
	jnz .l_file_close_err
	ret

	.l_file_close_err:
		; print errno code
		push rax
		mov rdi, str_err_code
		mov rsi, rax
		xor eax, eax
		call printf
		pop rax

		mov edi, str_info_fclose_fail
		jmp print_info_msg	; tailcall


; Gets fixed screen info line length and returns it in rax
; @param rdi - framebuffer file descriptor
;
get_line_len:
	; reseriving place for incoming struct
	sub rsp, FBIOGET_FSCREENINFO_SIZE
	
	mov rax, SYS_IOCTL
	mov rsi, FBIOGET_FSCREENINFO 
	mov rdx, rsp
	syscall

	cmp rax, 0
	jl .l_info_read_err

	xor rax, rax
	mov eax, dword[rsp + FSCREENINFO_LINE_LENGTH_OFFSET]
	
	; free memory
	add rsp, FBIOGET_FSCREENINFO_SIZE
	ret

	.l_info_read_err:
		push rax
		mov rdi, str_err_code
		mov rsi, rax
		xor eax, eax
		call printf
		pop rax

		add rsp, FBIOGET_FSCREENINFO_SIZE ; free memory
		mov rdi, str_ioctl_finfo_err
		jmp quit_with_error ; tailcall


; Gets variable screen info and stores it in the scr_info struct
; @param rdi - framebuffer file descriptor
; @param rsi - ptr to 24-byte space for scr_info struct
;
get_var_scr_info:
	; reserving place for incoming struct
	mov r8, rsi
	sub rsp, FBIOGET_VSCREENINFO_SIZE

	mov rax, SYS_IOCTL
	mov rsi, FBIOGET_VSCREENINFO 
	mov rdx, rsp
	syscall

	cmp rax, 0
	jl .l_info_read_err

	xor rax, rax
	mov eax, dword[rsp + VSCREENINFO_XRES_OFFSET]
	mov dword[r8 + SCRINFO_XRES], eax
	mov eax, dword[rsp + VSCREENINFO_YRES_OFFSET]
	mov dword[r8 + SCRINFO_YRES], eax
	mov eax, dword[rsp + VSCREENINFO_XOFFSET_OFFSET]
	mov dword[r8 + SCRINFO_XOFFSET], eax
	mov eax, dword[rsp + VSCREENINFO_YOFFSET_OFFSET]
	mov dword[r8 + SCRINFO_YOFFSET], eax
	mov eax, dword[rsp + VSCREENINFO_BPP_OFFSET]
	mov dword[r8 + SCRINFO_BPP], eax

	; free memory
	add rsp, FBIOGET_VSCREENINFO_SIZE
	ret

	.l_info_read_err:
		add rsp, FBIOGET_VSCREENINFO_SIZE ; free memory
		
		push rax
		mov rdi, str_err_code
		mov rsi, rax
		xor eax, eax
		call printf
		pop rax

		mov rdi, str_ioctl_vinfo_err
		jmp quit_with_error ; tailcall


; Returns screen buffer size
; @param edi - line_length
; @param esi - yres
;
get_screen_buffer_size:
	mov eax, edi
	mul esi
	ret


; Tries to map framebuffer file into process memory.
; Returns pointer to memory block mapped by mmap syscall in rax
; and the size of that block in rdx.
; @param rdi - framebuffer file descriptor
; @param rsi - ptr to scr_info struct
;
map_framebuffer:	
	mov rcx, rsi

	push rdi
	mov edi, dword [rcx + SCRINFO_LINE_LEN] ; line_length
	mov esi, dword [rcx + SCRINFO_YRES]		; yres

	call get_screen_buffer_size
	pop r8

	push rax
	xor rdi, rdi
	mov rsi, rax
	mov rdx, PROT_READ
	or rdx, PROT_WRITE
	mov r10, MAP_SHARED
	xor r9, r9

	mov rax, SYS_MMAP
	syscall
	pop rdx

	cmp rax, MAP_FAILED
	je .l_mmap_err
	ret

	.l_mmap_err:
		push rax
		mov rdi, str_err_code
		mov rsi, rax
		xor eax, eax
		call printf
		pop rax

		mov rdi, str_mmap_err
		jmp quit_with_error ; tailcall
		

; Unmaps previously allocated framebuffer memory
; @param rdi - ptr to mapped framebuffer memory
; @param rsi - size of framebuffer block
;
unmap_framebuffer:
	mov rax, SYS_MUNMAP
	syscall

	cmp rax, 0
	jnz .l_munmap_err
	ret

	.l_munmap_err:
		push rax
		mov rdi, str_err_code
		mov rsi, rax
		xor eax, eax
		call printf
		pop rax

		mov rdi, str_munmap_err
		jmp quit_with_error ; tailcall


; Uses sys_brk to get the current break address and
; return it in rax
;
get_curr_brk:
	mov rax, SYS_BRK
	xor rdi, rdi
	syscall
	ret


; Tries to allocate given amount of bytes on the heap;
; Returns address to allocated memory on success
; @param rdi - number of bytes to allocate
;
my_malloc:
	mov rax, SYS_BRK 
	add rdi, qword[current_break_ptr]
	syscall
	
	cmp rax, qword[current_break_ptr]
	je .l_alloc_err

	mov rdx, qword[current_break_ptr]
	mov qword[current_break_ptr], rax
	mov rax, rdx
	ret

	.l_alloc_err:
		mov rdi, str_brk_err
		jmp quit_with_error 	; tailcall


; Resets break address to its initial value.
; Should free all memory previously allocated with my_malloc
;
my_free_all:
	mov    rax, SYS_BRK
	mov    rdi, qword[initial_break_ptr]
	syscall
	mov    qword[current_break_ptr], rax
	ret


; Copies given number of bytes from source to destination
; @param rdi - destination ptr
; @param rsi - source ptr
; @param rdx - number of bytes to copy
;
my_memcpy:
        mov     rcx, rdx
        rep     movsb
        ret          


; Reads terminal parameters to the given termios struct
; @param rdi - ptr to termios struct
;
read_stdin_termios:
        mov rax, SYS_IOCTL
        mov rdx, rdi
        mov rdi, fd_stdin
        mov rsi, TCGETS
        syscall

		cmp rax, 0
		jl .l_read_err
        ret

		.l_read_err:
			push rax
			mov rdi, str_err_code
			mov rsi, rax
			xor eax, eax
			call printf
			pop rax

			mov rdi, str_ioctl_tcgets_err
			jmp quit_with_error ; tailcall


; Stores terminal parameters obtained from the given termios struct
; @param rdi - ptr to termios struct
;
write_stdin_termios:
        mov rax, SYS_IOCTL
        mov rdx, rdi
        mov rdi, fd_stdin
        mov rsi, TCSETS
        syscall

		cmp rax, 0
		jl .l_store_err
        ret

		.l_store_err:
			push rax
			mov rdi, str_err_code
			mov rsi, rax
			xor eax, eax
			call printf
			pop rax

			mov rdi, str_ioctl_tcsets_err
			jmp quit_with_error ; tailcall


; Sets the canonical flag in current termial params to the given value.
; @param rdi - new canonical flag value : 0 is false, non-zero is true 
;
set_canonical_flag:
		push rdi
		mov rdi, qword termios
        call read_stdin_termios
		pop rdi

		cmp rdi, 0
		je .l_unset

		; set canonical bit in local mode flags
        or dword [termios + 12], ICANON
		jmp .l_store_params

		.l_unset:
        	; clear canonical bit in local mode flags
        	and dword [termios + 12], ~ICANON

		.l_store_params:
			sub rsp, 8
			mov rdi, qword termios
        	call write_stdin_termios
			add rsp, 8

        ret


; Sets the echo flag in current termial params to the given value.
; @param rdi - new canonical flag value : 0 is false, non-zero is true 
;
set_echo_flag:
	push rdi
	mov rdi, qword termios
    call read_stdin_termios
	pop rdi

	cmp rdi, 0
	je .l_unset

    ; set echo bit in local mode flags
    or dword [termios + 12], ECHO
	jmp .l_store_params
    
	.l_unset:
		; clear echo bit in local mode flags
        and dword [termios+12], ~ECHO

	.l_store_params:
		sub rsp, 8
		mov rdi, qword termios
		call write_stdin_termios
		add rsp, 8

	ret


; Sets the O_NONBLOCK flag on stdin
; @param rdi - value to set the flag to: 0 for false, non-zero for true
;
set_stdin_nonblock:
	push rdi

	; get current flags
	mov rax, SYS_FCNTL
	mov rdi, fd_stdin
	mov rsi, F_GETFL
	syscall

	pop rcx

	cmp rcx, 0
	je .l_unset_flag

	or rax, O_NONBLOCK
	jmp .l_store_flags

	.l_unset_flag:
		and rax, ~O_NONBLOCK
	
	; save the modified flags
	.l_store_flags:
		mov rdx, rax
		mov rax, SYS_FCNTL
		mov rdi, fd_stdin
		mov rsi, F_SETFL
		syscall

	ret


; Disables cursor blinking in the terminal
;
disable_cursor_blink:
	push rax

	; magic string - standard control sequence to turn off cursor blinking on terminals
	mov rdi, str_no_blink
	xor eax, eax
	call printf

	pop rax
	ret


; Reads a single byte from stdin and returns it in rax.
; When stdin is set to NONBLOCKING, it may return -11 (EAGAIN)
; if there are no bytes ready to be read at the moment.
;
read_stdin_byte:
	sub         rsp, 8			; allocate 8-byte space on the stack as read buffer
	
	mov         rax, SYS_READ
	mov         rdi, fd_stdin
	mov         rsi, rsp		; set const char *buf to the 8-byte space on stack
	mov         rdx, 1			; set size_t count to 1 for one char
	syscall

	cmp rax, EAGAIN
	je .l_return

	movzx rax, byte[rsp]
	
	.l_return:
		add rsp, 8
		ret


; Draws a single pixel on the frame buffer
; @param edi - xpos
; @param esi - ypos
; @param rdx - color (4 byte, BGRA format)
; @param rcx - ptr to drawing_ctx struct
;
draw_pixel:
	push rbp
	push r12
	push r13

	mov rbp, qword[rcx + DRAWINGCTX_SCR_INFO]

	; sanity checks
	cmp edi, 0
	jl .l_sanity_check_failed
	cmp edi, dword[rbp + SCRINFO_XRES]
	jge .l_sanity_check_failed

	cmp esi, 0
	jl .l_sanity_check_failed
	cmp esi, dword[rbp + SCRINFO_YRES]
	jge .l_sanity_check_failed

	jmp .l_sanity_check_ok

	.l_sanity_check_failed:
		pop r13
		pop r12
		pop rbp
		ret

	.l_sanity_check_ok:
		mov r12, qword[rcx + DRAWINGCTX_FRAMEBUFFER_PTR]
		mov r13, rdx

		mov ecx, dword[rbp + SCRINFO_XOFFSET]	; xoffset
		mov edx, dword[rbp + SCRINFO_YOFFSET]	; yoffset
		mov r8d, dword[rbp + SCRINFO_BPP]		; bits per pixel
		shr r8d, 3								; make it bytes per pixel
		mov r9d, dword[rbp + SCRINFO_LINE_LEN]	; line_length

		; ecx := (x + xoffset) * bytes_per_pixel
		mov eax, edi
		add eax, ecx
		mul r8d
		mov ecx, eax

		; eax := (y + yoffset) * line_length
		mov eax, esi
		add eax, edx
		mul r9d

		; rax will store calculated location of pixel data
		add rax, rcx
		add rax, r12

		; set color
		mov qword[rax], r13

		pop r13
		pop r12
		pop rbp
		ret


; Draws a rectangle on a framebuffer
; @param rdi - ptr to rectangle struct
; @param rsi - color (4 byte, ARGB format)
; @param rdx - ptr to drawing_ctx struct
;
draw_rectangle:
	push rbp
	push r15
	push r14
	push r13
	push r12

	sub rsp, 16
	mov qword[rsp], rsi 		; put color on the stack
	mov qword[rsp + 8], rdx		; put drawing_ctx on the stack

	mov r12d, dword[rdi + RECT_XPOS]	; current xpos
	mov r13d, dword[rdi + RECT_YPOS]	; current ypos

	mov ebp, r12d	; store the original xpos in ebx

	mov r14d, r12d
	add r14d, dword[rdi + RECT_WIDTH]	; desired xpos
	mov r15d, r13d
	add r15d, dword[rdi + RECT_HEIGHT]	; desired ypos
	
	.l_draw_loop:
		mov rdi, r12				; xpos
		mov rsi, r13				; ypos
		mov rdx, qword[rsp]			; color
		mov rcx, qword[rsp + 8]		; drawing_ctx ptr
		call draw_pixel

		; go to the next pixel in a row
		add r12d, 1

		; check if exceeded row length
		cmp r12d, r14d
		jl .l_draw_loop

		; if exceeded current row length,
		; move to the next row
		add r13d, 1
		mov r12d, ebp
		
		.l_loop_condition:
			cmp r13d, r15d
			jl .l_draw_loop

	; free stack memory
	add rsp, 16

	pop r12
	pop r13
	pop r14
	pop r15
	pop rbp

	ret


; Checks if two segments collide.
; Returns 1 in rax if collision occurs or 0 otherwise.
; @param edi - beginning of 1st segment
; @param esi - end of 1st segment
; @param edx - beginning of 2nd segment 
; @param ecx - end of 2nd segment
;
check_segment_collision:
	cmp edi, ecx
	jg .l_no_collision
	
	cmp edx, esi
	jg .l_no_collision

	mov rax, 1
	ret 

	.l_no_collision:
		mov rax, 0
		ret


; Checks if two rectangles collide. Returns 1 in rax
; if the collision occures or 0 otherwise.
; @param rdi - ptr to first rectangle
; @param rsi - ptr to second rectangle
; 
check_rect_collision:
	push rbp
	push r12
	push r13

	mov rbp, rdi
	mov r12, rsi

	; rectangles are axis-alligned,
	; we divide the problem to
	; segment intersection checks
	mov edx, dword[rdi + RECT_XPOS]
	mov ecx, edx
	mov ecx, dword[rdi + RECT_WIDTH]

	mov esi, dword[rbp + RECT_XPOS]
	mov edi, esi
	add edi, dword[rbp + RECT_WIDTH]
	mov edx, dword[r12 + RECT_XPOS]
	mov ecx, edx
	add ecx, dword[r12 + RECT_WIDTH]
	call check_segment_collision

	cmp rax, 0
	je .l_return	; early return

	mov esi, dword[rbp + RECT_YPOS]
	mov edi, esi
	add edi, dword[rbp + RECT_HEIGHT]
	mov edx, dword[r12 + RECT_YPOS]
	mov ecx, edx
	add ecx, dword[r12 + RECT_HEIGHT]
	call check_segment_collision

	.l_return:
		pop r13
		pop r12
		pop rbp

		ret


; Sets the posiiton of a rectangle to given (x, y) coordinates.
; @param rdi - ptr to rectangle
; @param esi - x
; @param edx - y
;
set_rect_pos:
	mov dword[rdi + RECT_XPOS], esi
	mov dword[rdi + RECT_YPOS], edx

	ret


; Sets up given player struct
; @param rdi - ptr to player struct
; @param rsi - ptr to scr_info
; @param edx - player_rect_size
;
setup_player:
	mov r8, rdi
	add r8, PLAYER_RECT

	mov ecx, edx
	shr ecx, 1

	mov eax, dword[rsi + SCRINFO_XRES]
	shr eax, 1
	sub eax, ecx

	mov dword[r8 + RECT_XPOS], eax

	mov eax, dword[rsi + SCRINFO_YRES]
	shr eax, 1
	sub eax, ecx

	mov dword[r8 + RECT_YPOS], eax
	
	fild dword[r8 + RECT_YPOS]
	fstp dword[rdi + PLAYER_Y_POS]

	mov dword[r8 + RECT_WIDTH], edx
	mov dword[r8 + RECT_HEIGHT], edx
	mov dword[rdi + PLAYER_Y_VEL], __float32__(0.0)

	ret


; Draws the background rectangle
; @param rdi - ptr to drawing_ctx struct
; @param rsi - ptr to background rectangle
;
draw_background:
	sub rsp, 8

	mov rdx, rdi
	mov rdi, rsi
	mov rsi, COLOR_GREY
	call draw_rectangle

	add rsp, 8
	ret


; Draws the player
; @param rdi - ptr to drawing_ctx struct
; @param rsi - ptr to player struct
;
draw_player:
	sub rsp, 8
	add rsi, PLAYER_RECT

	mov rdx, rdi
	mov rdi, rsi
	mov rsi, COLOR_YELLOW
	call draw_rectangle

	add rsp, 8
	ret


; Draws the main scene.
; @param rdi - ptr to drawing_ctx struct
; @param rsi - ptr to player struct
; @param rdx - ptr to background rectangle
;
draw_main_scene:
	sub rsp, 24
	mov qword[rsp], rdi
	mov qword[rsp + 8], rsi

	mov rsi, rdx
	call draw_background

	mov rdi, qword[rsp]
	mov rsi, qword[rsp + 8]
	call draw_player

	add rsp, 24
	ret


; Sets up the physics context struct.
; @param rdi - ptr to physics context struct
;
setup_physics:
	mov dword[rdi + PHYSCTX_GRAVITY_ACC], __float32__(9.81)
	mov dword[rdi + PHYSCTX_JUMP_VEL], __float32__(250.0)
	mov dword[rdi + PHYSCTX_TIME_COEFF], __float32__(0.000001)
	mov dword[rdi + PHYSCTX_ACC_TUNE], __float32__(40.0)
	ret


; Simulates the player position based on received delta time.
; @param rdi - ptr to player struct
; @param rsi - ptr to physics context struct
; @param rdx - delta time
;
simulate_player:
	sub rsp, 16							; reserve space on stack
	mov qword[rsp], rdx					; store delta time
	
	; adjust the time
	fild qword[rsp]
	fld dword[rsi + PHYSCTX_TIME_COEFF]
	fmulp
	fstp qword[rsp]

	; update displacement
	fld dword[rdi + PLAYER_Y_POS]			; push y
	fld dword[rdi + PLAYER_Y_VEL]			; push v
    fld qword[rsp]							; push t
    fmulp									; v * t
	faddp									; y += v * t
	fst dword[rdi + PLAYER_Y_POS]			; save new y_pos
	fistp dword[rsp + 8]					; cast to integer and store

	; update velocity
	fld dword[rdi + PLAYER_Y_VEL]			; push v
	fld dword[rsi + PHYSCTX_GRAVITY_ACC]	; push a_orign
	fld dword[rsi + PHYSCTX_ACC_TUNE]		; push a_tune
	fmulp									; a = a_origin * a _tune
    fld qword[rsp]							; push t
    fmulp									; a * t
	faddp									; v += a * t
	fstp dword[rdi + PLAYER_Y_VEL]

	; set rectangle position
	add rdi, PLAYER_RECT
	mov esi, dword[rdi + RECT_XPOS]
	mov edx, dword[rsp + 8]
	sub rsp, 8
	call set_rect_pos
	add rsp, 8

	add rsp, 16

	ret


; Simulates player's jump.
; @param rdi - ptr to player struct
; @param rsi - ptr to physics context struct
;
player_jump:
	fld dword[rsi + PHYSCTX_JUMP_VEL]
	fchs
	fstp dword[rdi + PLAYER_Y_VEL]

	ret


; Updates the delta frame and last frame timestamp.
; Returns delta frame time in rax.
; @param rdi - ptr to last frame time
; @param rsi - ptr to delta frame time
;
update_time:
	push rbp
	push rdi
	push rsi

	; get current time
	call get_time_usec
	mov rbp, rax

	pop rsi
	pop rdi

	; and calculate the delta
	sub rax, qword[rdi]	
	mov qword[rsi], rax

	; then update the last frame time
	mov qword[rdi], rbp

	pop rbp
	ret


; Update action cooldowns
;	@param rdi - delta frame time
;
update_cooldowns:
	mov rax, qword[time_since_last_jump]
	add rax, rdi
	mov qword[time_since_last_jump], rax
	
	ret


; Increments given byte, but does not overflow
; @param rdi - ptr to 1 byte value
;
increment_byte_no_overflow:
	movzx ax, byte[rdi]
	add ax, 1
	cmp ax, 255
	jle .l_result_ok
	mov al, 255
	ret

	.l_result_ok:
		mov byte[rdi], al
		ret


; Reacts to user input
; @param rdi - input key code
;
handle_user_input:
	push rbp
	mov rbp, rdi

	; KEY_SPACE
	cmp rbp, KEY_SPACE
	jne .l_key_space_not_pressed
	cmp byte[ticks_since_keypress_space], byte KEYPRESS_COOLDOWN
	mov byte[ticks_since_keypress_space], 0
	jb .l_key_space_handled

	call player_jump_action
	jmp .l_key_space_handled

	.l_key_space_not_pressed:
		mov rdi, ticks_since_keypress_space
		call increment_byte_no_overflow

	.l_key_space_handled:

	; KEY_Q
	cmp rbp, KEY_Q
	jne .l_key_q_not_pressed
	cmp byte[ticks_since_keypress_q], byte KEYPRESS_COOLDOWN
	mov byte[ticks_since_keypress_q], 0
	jb .l_key_q_handled

	call quit_action
	jmp .l_key_q_handled

	.l_key_q_not_pressed:
		mov rdi, ticks_since_keypress_q
		call increment_byte_no_overflow

	.l_key_q_handled:

	pop rbp
	ret


; Performs player jump action.
;
player_jump_action:
	mov rax, qword[time_since_last_jump]
	cmp rax, JUMP_COOLDOWN
	jge .l_cooldown_passed
	ret

	.l_cooldown_passed:
		sub rsp, 8
		mov rdi, player
		mov rsi, physics_ctx
		call player_jump
		add rsp, 8

		mov qword[time_since_last_jump], 0
		ret


; Performs program exit action
;
quit_action:
	; exit normally
	mov rdi, 0
	jmp exit	; tailcall


; Does the cleanup and exits the program with given code.
; @param rdi - exit code
;
exit:
	; save the exit code
	push rdi

	; restore the framebuffer
	.l_restore_framebuffer:
		; check framebuffer pointers
		mov rax, qword[framebuffer_backup_ptr]
		cmp rax, 0
		je .l_unmap_framebuffer
		mov rax, qword[framebuffer_ptr]
		cmp rax, 0
		je .l_unmap_framebuffer

		mov rdi, qword[framebuffer_ptr]
		mov rsi, qword[framebuffer_backup_ptr]
		mov rdx, qword[framebuffer_size]
		call my_memcpy
		
	; unmap framebuffer from memory
	.l_unmap_framebuffer:
		mov rax, qword[framebuffer_ptr]
		cmp rax, 0
		je .l_close_framebuffer_fd

		mov rdi, qword [framebuffer_ptr]
		mov rsi, qword [framebuffer_size]
		call unmap_framebuffer

	; close framebuffer file
	.l_close_framebuffer_fd:
		mov eax, dword[fd_framebuffer]
		cmp eax, 0
		jl .l_free_all_memory

		mov rdi, [fd_framebuffer]
		call close_file	

	; free memory
	.l_free_all_memory:
		mov rax, qword[initial_break_ptr]
		cmp rax, 0
		je .l_quit		

		call my_free_all

	; set canonical flag back in terminal params
	mov rdi, 1 
	call set_canonical_flag

	; and set echo flag back in terminal params
	mov rdi, 1
	call set_echo_flag

	; and remove the NONBLOCKING flag from stdin
	mov rdi, 0
	call set_stdin_nonblock

	; quit program
	.l_quit:
	pop rdi			; exit code goes back to rdi

	mov eax, SYS_EXIT
	syscall
	ret			; probably not necessary, but...


; ----- START -----

_start: 
	; RSP is already 16-bit alligned here

	; get current break address and store it
	call get_curr_brk
	mov qword[initial_break_ptr], rax
	mov qword[current_break_ptr], rax

	; set up the PRNG seed using sys_time
	call get_sys_time
	mov dword [prng_seed], eax

	; open framebuffer file and store the FD
	mov rdi, file_framebuffer
	call open_file_rdwr
	mov dword [fd_framebuffer], eax

	; get line length and store it
	mov rdi, qword [fd_framebuffer]
	call get_line_len
	mov dword [scr_info + SCRINFO_LINE_LEN], eax

	; get variable screen info data and store it
	mov rdi, qword [fd_framebuffer]
	mov rsi, qword scr_info
	call get_var_scr_info

	; try to map the framebuffer into memory
	mov rdi, qword [fd_framebuffer]
	mov rsi, scr_info
	call map_framebuffer

	; store the ptr to mapped memory and
	; save the size of allocated memory block
	mov qword[framebuffer_ptr], rax
	mov qword[framebuffer_size], rdx

	; allocate memory and copy the framebuffer there
	; to be able to restore it on program exit
	mov rdi, qword[framebuffer_size]
	call my_malloc
	mov qword[framebuffer_backup_ptr], rax
	
	mov rdi, qword[framebuffer_backup_ptr]
	mov rsi, qword[framebuffer_ptr]
	mov rdx, qword[framebuffer_size]
	call my_memcpy

	; to read a single character without waiting
	; for the user to press enter, we must first
	; disable canonical mode
	xor rdi, rdi
	call set_canonical_flag

	; also disable the echo in the terminal
	xor rdi, rdi
	call set_echo_flag

	; set the NONBLOCKING flag for stdin
	mov rdi, 1
	call set_stdin_nonblock

	; disable cursor blinking in the terminal
	call disable_cursor_blink

	; setup the drawing context
	mov qword[drawing_ctx + DRAWINGCTX_SCR_INFO], scr_info
	mov rax, qword[framebuffer_ptr]
	mov qword[drawing_ctx + DRAWINGCTX_FRAMEBUFFER_PTR], rax

	; setup background rectangle
	mov dword[rect_background + RECT_XPOS], 0
	mov dword[rect_background + RECT_YPOS], 0
	mov eax, dword[scr_info + SCRINFO_XRES]
	mov dword[rect_background + RECT_WIDTH], eax
	mov eax, dword[scr_info + SCRINFO_YRES]
	mov dword[rect_background + RECT_HEIGHT], eax

	; setup the player
	mov rdi, player
	mov rsi, scr_info
	mov edx, dword[const_player_rect_size]
	call setup_player

	; setup physics context
	mov rdi, physics_ctx
	call setup_physics

	; allocate some variables on the stack
	sub rsp, 16

	call get_time_usec
	mov qword[rsp], rax		; last frame time
	mov qword[rsp + 8], 0	; delta frame time

	;;;
	.l_main_loop:
		; update time
		mov rax, rsp
		mov rdi, rax
		add rax, 8
		mov rsi, rax
		call update_time

		; update cooldowns
		mov rdi, rax
		call update_cooldowns

		; update the player
		mov rdi, player
		mov rsi, physics_ctx
		mov rdx, qword[rsp + 8]
		call simulate_player

		; draw the scene
		mov rdi, drawing_ctx
		mov rsi, player
		mov rdx, rect_background
		call draw_main_scene

		; read user input
		call read_stdin_byte

		; handle input
		mov rdi, rax
		call handle_user_input

		jmp .l_main_loop
		
	;;;

	.l_program_exit:
	; free stack memory
	add rsp, 16

	; exit normally
	mov rdi, 0
	call exit

