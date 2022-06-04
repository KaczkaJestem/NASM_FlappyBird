; Flappy Bird Game written in x64 NASM
; Target: 64-bit Linux
;
; Author: Adrian Kiełbowicz
;

; ===== EXTERNALS =====

extern printf

; ===== BSS SECTION =====

section		.bss

info_line_len		resb	4

; Variable Screen Info struct:
; resx : dw
; resy : dw
; xoffset : dw
; yoffset : dw
; bits_per_pixel : dw
;
var_scr_info		resb	20

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
str_mmap_err			db	'MMAP syscall failed - cannot map framebuffer into memory!', 0xa, 0
str_munmap_err			db	'MUNMAP syscall failed - cannot unmap framebuffer from memory!', 0xa, 0
str_brk_err				db	'BRK syscall failed. Cannot allocate memory!', 0xa, 0
str_err_code			db	'Error code: %d', 0xa, 0

str_title				db	'Flappy Bird', 0
str_play				db	'Play!', 0
str_quit				db	'Quit', 0

file_framebuffer		db	'/dev/fb0', 0

; ===== DATA SECTION =====

section		.data

; ----- Pointers -----

; we make them "null pointers" at the beginning
framebuffer_ptr			dq	0
framebuffer_backup_ptr	dq	0
initial_break_ptr		dq	0
current_break_ptr		dq	0

; ----- File descriptors -----
fd_framebuffer		dd	-1

; ----- Pseudo randomness -----
prng_seed		dd	0

; ----- Equate Directives -----
SYS_OPEN    	equ 2
SYS_CLOSE		equ 3
SYS_MMAP		equ 9
SYS_MUNMAP		equ 11
SYS_BRK			equ 12
SYS_IOCTL		equ 16
SYS_EXIT		equ 60
SYS_TIME		equ 201

O_RDONLY    	equ 0
O_WRONLY		equ 1
O_RDWR			equ 2

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

PROT_READ	equ 1
PROT_WRITE	equ 2

MAP_SHARED	equ 1

MAP_FAILED	equ -1

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


; Returns system time in rax; used mainly as seed for prng
; 
get_sys_time:
	; We use system call sys_time,
	; by passing 0 as the param we obtain the result in rax
        mov     rax, SYS_TIME
        xor     rdi, rdi         
        syscall
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
		mov rdi, [str_ioctl_finfo_err]
		jmp quit_with_error ; tailcall


; Gets variable screen info line length and stores it
; in the var_scr_info struct
; @param rdi - framebuffer file descriptor
; @param rsi - ptr to 20-byte space for var_scr_info struct
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
	mov dword[r8], eax
	mov eax, dword[rsp + VSCREENINFO_YRES_OFFSET]
	mov dword[r8+4], eax
	mov eax, dword[rsp + VSCREENINFO_XOFFSET_OFFSET]
	mov dword[r8+8], eax
	mov eax, dword[rsp + VSCREENINFO_YOFFSET_OFFSET]
	mov dword[r8+12], eax
	mov eax, dword[rsp + VSCREENINFO_BPP_OFFSET]
	mov dword[r8+16], eax

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

		mov rdi, [str_ioctl_vinfo_err]
		jmp quit_with_error ; tailcall


; Returns screen buffer size
; @param edi - xres
; @param esi - yres
; @param edx - bits per pixel
;
get_screen_buffer_size:
	mov eax, edx
	shr eax, 3
	mul edi
	mul esi
	ret


; Tries to map framebuffer file into process memory.
; Returns pointer to memory block mapped by mmap syscall in rax
; and the size of that block in rdx.
; @param rdi - framebuffer file descriptor
; @param rsi - ptr to var_scr_info struct
;
map_framebuffer:	
	mov rcx, rsi

	push rdi
	mov edi, dword [rcx] 		; xres
	mov esi, dword [rcx + 4]	; yres
	mov edx, dword [rcx + 16]	; bits per pixel

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

		mov rdi, [str_mmap_err]
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

		mov rdi, [str_munmap_err]
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
		mov rdi, qword[str_brk_err]
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
	mov dword [info_line_len], eax

	; get variable screen info and store it
	mov rdi, qword [fd_framebuffer]
	mov rsi, qword var_scr_info
	call get_var_scr_info

	; try to map the framebuffer into memory
	mov rdi, qword [fd_framebuffer]
	mov rsi, var_scr_info
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

	;;;

	;;;

	; exit normally
	mov rdi, 0
	call exit

