BITS 64
extern main

section .data

section .text
    global _start

_start:
	pop rdi
	pop rsi
	call main
	mov rdi, rax ; store the returned value in rdi, this is the error code
	mov rax, 60 ; exit
    syscall
