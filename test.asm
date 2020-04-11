Data 	Segment use16
	Vb		db	1h
	Vd		dd	0d5h
	String1		db	"Assembler"
	Times1		EQU 	10
	Times2		EQU 	0
Data 	ends


label1:
	jz	lend
	add	vb[ebp*8],56h
	mov	eax, Vd
	dec 	eax
	push	100
	mov	esi, 1
	test	eax,FS:Vd[esi*2] 
	bts	Vd[esi*4], eax
	add	String1[esi*2], 1
	IF Times1
		M1 dd 7ah
		test eax, M1
	ENDIF
	IF Times2
		mov	edi, String1
	ENDIF
lend:
	stosd
	cmp	eax, esi
	test 	bh,vd[esi*4]
	jz label1
	add al,bl
	and eax,tbx
Code 	ends
END

