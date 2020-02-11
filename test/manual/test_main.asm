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
label_8: db ' (%d %d) ', 0x0
[section .text]
global main
extern mark
extern allocObject
extern printf
main:
push rbp
mov rbp, rsp
sub rsp, 40
mov [rbp - 8], r10
push rbx
lea rbx, [rbp - 16]
mov [rbp - 40], rbx
mov r10, rbp
mov rdi, label_7_Human
xor rax, rax
call allocObject
mov rbx, rax
mov rcx, rbx
add rcx, 16
mov rcx, [rcx]
mov [rbp - 24], rcx
mov rcx, [rbp - 24]
mov [rbp - 32], rcx
mov r10, rbp
mov rdi, label_6_Pair
xor rax, rax
call allocObject
mov rcx, rax
add rcx, 16
mov rcx, [rcx]
mov qword [rcx], 123
mov rcx, rax
add rcx, 16
mov rcx, [rcx]
mov qword [rcx + 8], 456
mov rcx, [rbp - 32]
mov [rcx], rax
mov rax, [rbp - 40]
mov [rax], rbx
mov r10, rbp
mov rdi, label_8
mov rax, [rbp - 16]
add rax, 16
mov rax, [rax]
mov rax, [rax]
add rax, 16
mov rbx, 0
add rbx, [rax]
xor rdx, rdx
mov rax, 3
mov rcx, 6
imul rcx
mov rcx, rax
mov rsi, [rbx]
add rsi, rcx
mov rbx, [rbp - 16]
add rbx, 16
mov rbx, [rbx]
mov rbx, [rbx]
add rbx, 16
mov rbx, [rbx]
mov rdx, [rbx + 8]
xor rax, rax
call printf
mov rax, 0
pop rbx
leave
ret