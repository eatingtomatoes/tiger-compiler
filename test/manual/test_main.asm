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
label_13: db "i = %d, i + i * 2 = %d", 0xa, "", 0x0
label_12: db "i = %d, i * 2 = %d", 0xa, "", 0x0
[section .text]
global main
extern mark
extern allocObject
extern printf
main:
push rbp
mov rbp, rsp
sub rsp, 48
mov [rbp - 8], r10
push rbx
mov rbx, 0
mov rax, -16
add rax, rbp
mov [rbp - 24], rax
label_8:
cmp rbx, 5
jnl label_9
mov rax, [rbp - 24]
mov [rbp - 40], rax
xor rdx, rdx
mov rax, rbx
mov rcx, 2
imul rcx
mov [rbp - 32], rax
label_10:
cmp qword [rbp - 48], 5
jnl label_11
mov rcx, [rbp - 32]
mov rdx, [rbp - 40]
mov [rdx], rcx
cmp rbx, 2
jnl label_15
mov rcx, [rbp - 40]
mov rdx, [rcx]
mov r10, rbp
mov rdi, label_12
mov rsi, rbx
xor rax, rax
call printf
label_16:
mov rcx, [rbp - 48]
add rcx, 1
mov [rbp - 48], rcx
jmp label_10
label_15:
mov rcx, [rbp - 40]
mov rcx, [rcx]
mov rdx, rbx
add rdx, rcx
mov r10, rbp
mov rdi, label_13
mov rsi, rbx
xor rax, rax
call printf
jmp label_16
label_11:
add rbx, 1
jmp label_8
label_9:
mov rax, 0
pop rbx
leave
ret