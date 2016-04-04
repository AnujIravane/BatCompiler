.text
.globl go
go:
pushq %rbx
pushq %rbp
pushq %r12
pushq %r13
pushq %r14
pushq %r15
call _main
popq %r15
popq %r14
popq %r13
popq %r12
popq %rbp
popq %rbx
retq
_main:
subq $16, %rsp
movq $7, %rdi
movq $5, %rsi
call allocate
movq %rax, 0(%rsp)
movq $9, %rdi
movq $5, %rsi
call allocate
movq %rax, 8(%rsp)
movq $_create_big_array_ret, -8(%rsp)
movq 0(%rsp), %rdi
movq 8(%rsp), %rsi
subq $8, %rsp
jmp _create_big_array
_create_big_array_ret:
movq %rax, %rdi
call print
addq $16, %rsp
ret
_create_big_array:
subq $0, %rsp
movq 0(%rdi), %r8
movq 0(%rsi), %r9
movq %r8, %rdi
addq %r9, %rdi
imulq $2, %rdi
addq $1, %rdi
movq $5, %rsi
call allocate
addq $0, %rsp
ret
