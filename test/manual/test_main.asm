[section .data]
label_7_Human:
dq 0x1234321
dq 1
dq label_6_Pair
label_6_Pair:
dq 0x1234321
dq 2
dq 0
dq 0
label_8: db '%d %d', 0x0
[section .text]
global main
extern mark
extern allocObject
extern printf
main:
push rbp
mov rbp, rsp
add rsp, -16
mov [rbp - 8], r10
push rbx
push r12
push r13
push r14
push r15
mov r12, -16
add r12, rbp
mov r10, rbp
mov rdi, label_7_Human
xor rax, rax
call allocObject
mov rbx, rax
mov rcx, rbx
add rcx, 16
mov r13, 0
add r13, [rcx]
mov r10, rbp
mov rdi, label_6_Pair
xor rax, rax
call allocObject
mov rcx, rax
mov rdx, rcx
add rdx, 16
mov rdx, [rdx]
mov qword [rdx + 0], 8
mov rdx, rcx
add rdx, 16
mov rdx, [rdx]
mov qword [rdx + 8], 9
mov [r13], rcx
mov [r12], rbx
mov r10, rbp
mov rdi, label_8
mov rbx, [rbp + -16]
add rbx, 16
mov rbx, [rbx]
mov rbx, [rbx + 0]
add rbx, 16
mov rcx, 0
add rcx, [rbx]
mov rsi, [rcx]
mov rbx, [rbp + -16]
add rbx, 16
mov rbx, [rbx]
mov rbx, [rbx + 0]
add rbx, 16
mov rcx, 8
add rcx, [rbx]
mov rdx, [rcx]
xor rax, rax
call printf
mov rax, 0
pop r15
pop r14
pop r13
pop r12
pop rbx
leave
ret