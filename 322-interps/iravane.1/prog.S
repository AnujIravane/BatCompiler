.text
.globl go
go:
pushq %rbx
pushq %rbp
pushq %r12
pushq %r13
pushq %r14
pushq %r15
call _go
popq %r15
popq %r14
popq %r13
popq %r12
popq %rbp
popq %rbx
retq
_go:
subq $0, %rsp
movq $-5, %rdi
movq $5, %rsi
cmpq %rsi, %rdi
setle %dil
movzbq %dil, %rdi
movq $1, %rbx
salq %bl, %rdi
addq $1, %rdi
call print
addq $0, %rsp
ret
