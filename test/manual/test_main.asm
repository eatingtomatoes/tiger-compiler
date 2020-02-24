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
sub rsp, 112
mov [rbp - 8], r10
push rbx
mov qword [rbp - 64], 0
mov qword [rbp - 32], -16
add [rbp - 32], rbp
mov qword [rbp - 24], 0
mov rbx, r8
mov r8, rax
label_8:
cmp qword [rbp - 24], 5
jnl label_9
mov rax, [rbp - 32]
mov [rbp - 56], rax
xor rdx, rdx
mov rax, [rbp - 24]
mov rdx, 2
imul rdx
mov [rbp - 48], rax
mov r10, [rbp - 64]
mov [rbp - 40], r10
mov [rbp - 80], rcx
mov [rbp - 72], rdi
mov [rbp - 112], r8
mov [rbp - 88], r9
label_10:
cmp qword [rbp - 40], 5
jnl label_11
mov rcx, [rbp - 48]
mov rdx, [rbp - 56]
mov [rdx], rcx
cmp qword [rbp - 24], 2
jnl label_15
mov rcx, [rbp - 56]
mov rcx, [rcx]
mov [rbp - 96], rcx
mov r10, rbp
mov rdi, label_12
mov rsi, [rbp - 24]
mov rdx, [rbp - 96]
xor rax, rax
call printf
mov rcx, rax
mov rdi, [rbp - 96]
mov r8, [rbp - 112]
mov r9, [rbp - 88]
label_16:
mov rsi, [rbp - 40]
add rsi, 1
mov [rbp - 40], rsi
mov [rbp - 80], rdi
mov [rbp - 72], rcx
mov [rbp - 112], r8
mov [rbp - 88], r9
jmp label_10
label_15:
mov rbx, [rbp - 56]
mov rbx, [rbx]
mov rcx, [rbp - 24]
mov [rbp - 104], rcx
add [rbp - 104], rbx
mov r10, rbp
mov rdi, label_13
mov rsi, [rbp - 24]
mov rdx, [rbp - 104]
xor rax, rax
call printf
mov r9, rax
mov rdi, [rbp - 80]
mov rcx, [rbp - 72]
mov r8, [rbp - 104]
jmp label_16
label_11:
mov rax, [rbp - 24]
add rax, 1
mov [rbp - 24], rax
mov rcx, [rbp - 80]
mov rdi, [rbp - 72]
mov r8, [rbp - 112]
mov r9, [rbp - 88]
jmp label_8
label_9:
mov rax, 0
pop rbx
leave
ret