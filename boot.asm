[bits 16]
[org 0x7c00]

SECTION boot1

start:
	xor ax, ax
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov bp, 0x7000
	mov sp, bp
	sub sp, 2

	call init_timer
	call set_video
	call init_fpu

	mov ax, 0xA000
	mov fs, ax
start0:
	call calc_angles
	xor bx, bx
	xor cx, cx
start1:
	xor dx, dx
start2:
	mov word [fs:bx], 65535
	call get_color
	mov [fs:bx], al
	inc bx
	inc dx
	cmp dx, 320
	jb start2

	inc cx
	cmp cx, 200
	jb start1
	jmp start0
end:
	cli
	hlt
	jmp end

; dx = screen_x
; cx = screen_y
get_color:
	push bx
	push cx
	push dx

	mov bx, 0x1002
	sub dx, 160
	sub cx, 100
	neg cx
	mov [bx], dx
	mov [bx+2], cx

	fild word [bx]
	fild word [i100]
	fdiv st1, st0

	fild word [bx+2]
	fdivrp ; st0: v, st1: u

	fld1
	fld st2
	fmul st0, st0 ; u*u, 1, v, u

	fld st2
	fmul st0, st0 ; v*v, u*u, 1, v, u

	faddp
	fadd st0, st1 ; 1 + u*u + v*v, 1, v, u
	fsqrt
	fdivp ; rsqrt(mag2(u,v,1)), v, u

	fmul st1, st0
	fmul st2, st0

	fstp qword [0x1018]
	fstp qword [0x1010]
	fstp qword [0x1008]

	fldz
	fst qword [0x1030]
	fst qword [0x1028]
	fst qword [0x1020]

	mov ax, 20
get_color0:
	call sdf
	faddp
	fld qword [0x1008]
	fmul st0, st1
	fstp qword [0x1020]
	fld qword [0x1010]
	fmul st0, st1
	fstp qword [0x1028]
	fld qword [0x1018]
	fmul st0, st1
	fstp qword [0x1030]

	dec ax
	jnz get_color0

	fmul st0, st0
	fld1
	fdivrp
	fimul word [i63]
	fistp word [bx]
	mov al, [bx]

	pop dx
	pop cx
	pop bx
	ret
i100:
dw 100
i63:
dw 63

c2 equ 0x1038
f_0 equ 0x1040
s2 equ 0x1048
s1s2 equ 0x1050
c1 equ 0x1058
c2s1 equ 0x1060
c1s2 equ 0x1068
s1 equ 0x1070
c1c2 equ 0x1078
;                       0  8 16  24  32   40    48  56  64
; double9 @ [0x1038] = {c2,0,s2,s1s2,c1,-c2s1,-c1s2,s1,c1c2}
calc_angles:
	push bx
	mov bx, 0x1038
	fldz
	fstp qword [bx+8] ; f_0
	fild word [bx-0x38]
	fidiv word [i256]
	fld st0
	fadd st0, st0
	fsincos
	fstp qword [bx] ; c2
	fstp qword [bx+16] ; s2
	fsincos
	fstp qword [bx+32] ; c1
	fst qword [bx+56] ; s1

	fld qword [bx+16] ; s2
	fmul st0, st1
	fstp qword [bx+24] ; s1s2
	fld qword [bx] ; c2
	fxch st1
	fmulp
	fchs
	fstp qword [bx+40] ; c2s1

	fld qword [bx+32] ; c1
	fld qword [bx+16] ; s2
	fmul st0, st1
	fchs
	fstp qword [bx+48] ; c1s2
	fld qword [bx] ; c2
	fmulp
	fstp qword [bx+64] ; c1c2

	pop bx
	ret
i256:
	dw 256

; ro = double3 @ [0x1020]
; pushes the return value on the fpu stack
sdf:
	mov di, 0x1020
	mov si, 0x1038
	mov cx, 3
sdf0:
	fld qword [di]
	fmul qword [si]
	fld qword [di+8]
	fmul qword [si+8]
	fld qword [di+16]
	fsub dword [f_9_4]
	fmul qword [si+16]
	faddp
	faddp
	fmul st0, st0
	add si, 24
	dec cx
	jnz sdf0
	; fpu stack should be [z^2, y^2, x^2]
	fxch st1 ; [y^2, z^2, x^2]
	fxch st2 ; [x^2, z^2, y^2]
	faddp
	fsqrt
	fld1
	fsubp
	fmul st0, st0
	faddp
	fsqrt
	fsub dword [f_1_4]
	ret
f_9_4:	dd 2.25
f_1_4:	dd 0.25

set_video:
	mov ax, 0x0013
	int 0x10
	mov dx, 0x3C8
	xor al, al
	out dx, al
	inc dx
set_video1:
	out dx, al
	out dx, al
	out dx, al
	inc al
	jnz set_video1

	ret

init_fpu:
	push eax
	mov eax, cr0
	and eax, ~((1 << 2) + (1 << 3))
	mov cr0, eax
	fninit
	pop eax

init_timer:
	push ax
	xor ax, ax
	cli
	mov word [0x0020], timer
	mov word [0x0022], ax
	mov ax, 4661
	out 0x40, al
	mov al, ah
	out 0x40, al
	mov al, 0b00000100
	out 0x43, al
	sti
	pop ax
	ret

timer:
	push ax
	inc word [0x1000]

	mov al, 0x20
	out 0x20, al
	pop ax
	iret
times (510 - ($ - $$)) db 0x00
dw 0xAA55
