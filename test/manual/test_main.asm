[section .data]
label_6: db '(%d %d %d) ', 0x0
[section .text]
global main
extern printf
main:
push rbp
mov rbp, rsp
add rsp, -8
mov [rbp - 8], r10
mov rdi, 6
mov r10, rbp
call label_1_printBoard
mov rax, 0
leave
ret
label_1_printBoard:
push rbp
mov rbp, rsp
add rsp, -24
mov [rbp - 8], r10
mov rsi, rdi
mov qword [rbp + -16], 1
label_2:
cmp qword [rbp + -16], 3
jnl label_3
mov qword [rbp + -24], 1
label_4:
cmp qword [rbp + -24], 3
jnl label_5
mov rax, -24
add rax, rbp
mov rcx, [rax]
mov rax, -16
add rax, rbp
mov rdx, [rax]
mov rdi, label_6
mov r10, rbp
call printf
add qword [rbp + -24], 1
mov rax, qword [rbp + -24]
mov [rbp + -24], rax
jmp label_4
label_5:
add qword [rbp + -16], 1
mov rax, qword [rbp + -16]
mov [rbp + -16], rax
jmp label_2
label_3:
leave
ret