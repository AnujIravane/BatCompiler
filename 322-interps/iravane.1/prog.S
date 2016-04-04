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
movq $5, %rdi
movq $_g_ret, -8(%rsp)
subq $8, %rsp
jmp _g
_g_ret:
addq $0, %rsp
ret
_g:
subq $0, %rsp
addq $7, %rdi
subq $1, %rdi
call print
addq $0, %rsp
ret
