.386

	max_prg    equ 10     ; макс. кількість одночасно вик. задач
	time_slice equ 65535  ; к-сть мікросекунд , виділених на квант часу
	
	_ST segment WORD STACK 'stack' use16
			dw 32000 dup(?)
		top label word  ; присвоїть адрес 
			dw 100 dup(?) ; резерв для помилок переповнення стеку
	_ST ends
	
	_DATA segment WORD PUBLIC 'DATA' use16
		@ms_dos_busy dd (?) ; логічна адреса зайнятості MS-DOS
		int8set      db 0   ; ознака перехоплення від таймера
		int33set      db 0   ; ознака перехоплення від мишки
		fon  equ max_prg    ; ознака фонової задач
		fonsp label word    ;адреса збереження SP фонової задачі
		sssp  dd top        ;логічна адреса стека фонової задачі
		
		;масив значень SP для задач (для стека кожної задачі 1000 слів)
		
		stp dw 1000,2000,3000,4000
			dw 5000,6000,7000,8000
			dw 9000,10000,11000,12000
			dw 13000,14000,15000,16000
		
		nprg  dw 0 ; номер активної задачі(від 0 до max_prq-1  ) , або ознака фонової задачі (fon)
		
		;масив стану задач
		init db 16 dup(0)
		
		;масив дозволеного числа квантів задач
		clock db 16 dup(1)
		
		;масив лічильників квантів задач
		clockt db 16 dup(0)
		
		screen_addr dw 16 dup(0); адреса (зміщення від початку відеосторінки) області виведення на екран значень задачі 
		
		; масив імен задачі
		names label word 
			db '0T1T2T3T4T5T6T7T8T9TATBTCTDTETFT'
			
		clk dw 0; лічильник переривань від таймера
		
	_DATA ends
	
_TEXT segment BYTE PUBLIC 'CODE' use16 
	assume CS:_TEXT , DS:_DATA
	
;процедура перехоплення переривань від таймера (int8)

setint8 proc
	mov al , int8set
	or al , al ;контоль перехоплення перехоплень
	jnz @zero_8 
	mov ah , 35h  ;отримати вектор переривання 
	mov al , 8    ;від таймера
	int 21h       ; отримуємо es:bx логічна адреса системної процедури
	
	mov cs:int8ptr , bx  ;зберегти логічну адресу системноїї процедури в сегменті кодів 
	mov cs:int8ptr+2 , es 
	
	mov dx , offset userint8 ; формування в ds:dx логічної адоеси процедури користувача
	push ds					;для обробки переривань від таймера
	push cs
	pop ds
	
	mov ah , 25h   ;встановити вектор переривань від таймера
	mov al , 8
	int 21h       ;маємо в ds:dx - покажчик на користувацьку процедуру обробки переривань від таймера 
	
	mov ax , time_slice  ; встановити задану величину кванту
	out 40h , al ;40h - адреса 8-розрядного порта таймера через який задають період таймера спочатку молодший байт , а потім старший
	jmp $+2 ; хитрий трюк - описан в методичці
	nop
	mov al , ah ; старший байт
	out 40h, al 
	
	pop ds
	mov int8set , 0ffh ;заборона повторних входжень
@zero_8:
	ret
	int8ptr dw 2 dup(?)
setint8 endp

;процедура відновлення вектора переривань від таймера

retint8 proc
	push ds
	push dx
	
	mov al , 0ffh  ;відновити нормальну роботу системного таймера
	out 40h , al 
	jmp $+2
	nop
	out 40h, al
	mov dx , cs:int8ptr
	mov ds , cs:int8ptr +2 
	
	mov ah , 25h ; відновити початковий вектор переривань від таймера
	mov al , 8  
	int 21h     ;ds:dx - вказівник(логічна адреса) на початкову (системну) процедуру оброб. переривання від таймера
	
	pop dx
	pop ds
	mov int8set, 0h ; дозівл наступних переривань
	ret
retint8 endp 

	
;процедура "перехоплення" переривань від мишки(int33)

setint33 proc
	mov al , int33set
	or al , al
	jnz @zero_9

	xor ax, ax
	int 33h
	
	mov ax , 1
	int 33h
	
	mov ax , 0ch
	mov cx , 1000b  ;права кнопка 
	push es
	push cs
	pop es
	lea dx , userint33
	int 33h
	pop es
 
	mov int33set , 0ffh  ;заборона повторних входжень

@zero_9:
	ret
setint33 endp

;процедура припинення 
retint33 proc
	push cx
	push ax  


	xor cx , cx
	mov ax , 0ch
	int 33h
	
	pop cx 
	pop ax
	mov int33set, 0h  ;дозвіл наступних "перехоплень"
	ret
retint33 endp


;процедура обробки подій від мишки
userint33 proc far
	push ds
	push es
	pusha 
	
	push 0b800h
	pop es
	
	push _DATA
	pop ds
	
@My_code:
	;узнаем положение
	shr dx , 3
	shr cx , 3
	
	imul dx , 80
	add dx , cx	 ; в dx  положення
	
	mov bx , 26 ;початкова верхня границя
	mov ax , 2  ;початкова нижня границя
	mov cx , max_prg  ; к-сть 
	
;цикл , що шукає область виводу в яку натиснуал мишка
@my_loop:	
	cmp dx , bx
	ja  @next_loop   ;перевіряємо вихід за верхню границю
	cmp dx , ax
	jl  @next_loop   ;за нижню 
	jmp @Work 
@next_loop:       
	add bx , 40   ;додаємо значення 
	add ax , 40
	loop @my_loop  
	jmp @exit	
@Work:
	mov bx , max_prg
	sub bx , cx       ;маємо номер процесу
	
	add clock[bx] , 10  ; додаємо до кількості квантів процесу константу
	jc @set_max    ; якщо маємо перебільшення 255 , то переходимо на set_max
	jmp @exit    ; якщо ні , то переходимо на вихід
@set_max:
	mov clock[bx] , 255
	
	
@exit:
	popa 
	pop es
	pop ds 	
	ret
userint33 endp
	
;процедура обробки переривань  від таймера
;(менеджер квантів)
; коди задач (викор в масиві init)
ready equ 0 ; задача завантажена в пам'ять і готова до початкового запуску статус вст. поза менеджером квантів

execute equ 1 ; задача виконується
hesitation equ 2 ; задача призупинена і чекає своєї черги
close equ 4; виконання задачі завершено
stop equ 8; задача зупинена стаус вст. і змінюється поза менеджером квантів
absent equ 16 ; задача вісутня

userint8 proc far
	pushad ; збереження РОН в стеку перерванної задачі
	push ds
;(варіант 3)
	pushf     ;програман
	
	call cs:dword ptr int8ptr
	;виклик системної процедури обробки переривання int8
	
	mov ax,_data ;в перерваній програмі вміст сегментного регістра
	mov ds , ax  ;ds любий в заг випадку
	
	inc clk  ; програмний лічильник переривань від таймера
	push clk ;може бути користним при вивчені моделі
	push  2440
	call show   ; виведення на екран значення лічильника 
	
	xor esi , esi
	mov si , nprg
	cmp si , fon ; перервана задача фонова 
	je @disp005
					;перервана задача не фонова
	
	 
	сmp init[si] , stop
	je @disp015
	cmp clockt[si], 1 ;є ще не використані кванти ? 
	jc @disp010
	
	dec clockt[si] ; зменшити лічильник квантів
	pop ds
	popad  ; продовжити виконання перерваної задачі  
	iret 
@disp005:  ;перервана задача фонова 
	mov fonsp , sp
	mov nprg , max_prg-1  ;забезпечити перегляд задач з 0-вої
	
	mov cx , max_prg ; max_prg  - max кількість задач 
	jmp @disp015
@disp010:
	mov stp[esi*2],sp
	mov init[si] , hesitation  ;призупинити поточну задачу 
	mov cx , max_prg
@disp015:
	;визначення задачі , якій необхідно перелати управління
	mov di , max_prg+1
	sub di , cx
	add di , nprg
	cmp di , max_prg
	jc @disp018
	sub di, max_prg
@disp018:
	xor ebx , ebx
	mov bx , di
	;push bx
	;push 3220  
	;call show
	; cx пробігає значення max_prg , max_prq-1 , ..., 1
	; bx пробігає значення nprg+1 , ... , max_prg-1 , 1, 0 , ... , nprg

	cmp init[bx] , ready
	je @disp100 ; перехід на початковий запуску задачі 
	
	cmp init[bx] , hesitation
	je @disp020  ;перехід на відновлення роботи наступної задачі
	loop @disp015
	
	;відсутні задачі , які можна запустить , або перезапустити , тому утстановлюємо стек фонової задачі
	mov sp , fonsp
	mov nprg , fon
	pop ds 
	popad
	iret

@disp020:
	;відновлення роботи наступної задачі
	;push bx
	;push 2480
	;call show
	mov nprg, bx
	mov sp,stp[ebx*2]
	mov al , clock[bx]
	mov clockt[bx] , al  ;встановити дозволену кількість квантів 
	mov init[bx] , execute  ;стан задачі - задача вик.
	
	pop ds
	popad 
	iret
@disp100:
	;першопочатковий запуск задачі
	mov nprg , bx
	mov sp , stp[ebx*2]
	mov al, clock[bx]
	mov clockt[bx] , execute
	
	push names[ebx*2] ;ім'я задачі 
	push screen_addr[ebx*2] ; адрес "вікна" для задачі на екрані 
	push 22   ;розрядність лічильника
	call Vcount ; запуск 
	
	xor esi , esi
	mov si , nprg  ; 3на ax - номер задачі , яка завершида свою роботу в межах чергового кванту часу 
	
	mov init[si] , close
	mov sp , fonsp
	mov nprg , fon
	pop ds
	popad
	iret ; повернення в фонову задачу

userint8 endp

;-
; Vcount - процедура для моделювання незалежних задач 
; вхідні параметри:
;	1-й - ім'я задачі (два символа) [bp+8]
;	2-й - зміщення в відеосторінці "вікна" задачі [bp+6]
;	3-й - кількість двійкових розрядів лічильника [bp+4]
; Виконувані дії:
;    при запуску:
;  - дозволяє переривання
;  - створює в стеку 10-байтну область для локальних даних
;  - розміщує в цю область по адресі [bp-2] статок від ділення
;               3-го параметра на 32 (фактична розрядність лічильника -
;               перестраховка від помилок в завданні розрядності)
;   - записує в цю область по адресу [bp-6] маску з числом
;               одиниць в молодших розрядів рівним фактичній 
;               розрядності лічильника
;  - записує в нуль в 4-х байт ний лічильник по адресу [bp-10]

;   в подальшому в циклі:
;           - виводить показники лічильника на екран
;           - збільшує значення лічильника на 1
;  завершення задачі після переходу лічильника 
;  з стану "всі одиниці" в стан всі 0

Vcount proc near
	push bp
	mov bp , sp
	sub sp,10 ; формування в стеку областы для збереження даних
	sti 
	
	push es
	mov ax , 0b800h
	mov es , ax
	
	mov ax , [bp+4]  ; ax - кілкість розрядів лічильника
	and ax , 31      ; ax = ax mod32 (для перестраховки)
	mov [bp-2], ax   ;по [bp-2] , кількість розр. лічильника  <32
	
	mov cx , ax
	mov eax , 001b
	shl eax , cl
	dec eax   ; eax - маска с чилои 1 рвним к-сті розрядів лічильника 
	
	mov [bp-6],eax
	
	mov dword ptr[bp-10],0 ;скидання лічильника
	mov di,[bp+6] ; вивід імені задачі
	mov dx, [bp+8]
	mov ah , 1010b
	mov al , dh
	cld
	stosw
	mov al, dl
	stosw
	
	std         ;підготовка до виводу лічильника  , починаючи з молодших розрядів 
	add di , cx
	add di , cx
	mov bx , di
	xor edx , edx
@120:    ;вивід показників лічильника в двоїчному форматі
	mov di , bx
	mov cx, [bp-2]
	mov ah , 1010b ;1010b атрибут символу , атрибут фога - 0(чорний)
@140:
	mov al, '0'
	shr edx , 1
	jnc @160
	mov al , '1'
@160:
	stosw
	loop @140
	
	inc dword ptr[bp-10] ; +1 в лічильник
	mov edx, dword ptr [bp-10]
	and edx,  [bp-6] ; перевіоока на 0
	jnz @120
	
	pop es
	add sp, 10
	mov ax,[bp+8]
	and ax , 0fh
	cli
	pop bp
	ret 6
Vcount endp
	
show proc near
		push bp
		mov bp , sp
		pusha
		push es
		mov ax , 0b800h
		mov es , ax
		
		std
@ls20:
	mov di , [bp+4]
	mov bx , [bp+6]
	mov cx, 4
	mov ah, 0ah
@ls40:
	mov al , bl
	and al , 00001111b
	cmp al , 10
	jl @ls100
	add al,7
@ls100:
	add al,30h
	stosw
	shr bx , 4
	loop @ls40
	
	pop es
	popa
	pop bp
	ret 4
show endp


begin:
	mov ax, _DATA
	mov ds,ax
	
	mov ax,3  ; текствоий режим
	int 10h
	
	
	
	mov ah , 10h  ;відключити миготіння
	mov al , 3
	mov bl, 0
	int 10h
	
	mov cx, max_prg
	xor esi , esi
	mov bx ,4
@b10:
	mov screen_addr[esi*2],bx ;заповнення таблиці
	mov init[si],ready
	add bx,80
	inc si
	loop @b10
;setint8
	cli
	mov ah,34h
	int 21h ;es:bx - адреса ознаки зайнятості MS-DOS
	mov word ptr @ms_dos_busy , bx 
	mov word ptr @ms_dos_busy+2 , es
	
	call setint8 ; перехоплення int8
	call setint33 ;перехоплення int9
	
	lss sp , sssp  ;стек фонової задачі 
	mov nprg , fon
	push 'FN'
	push 1800
	push 30
	call Vcount  ;запуск фонової задачі
				 ; в процедурі Vcount установлюється дозвіл
				;на переривання і при чергових перериваннях
				; від таймера менеджер квантів (userint8) 
				; буде запускать інші задачі
;управління в цю точку буде передано по команді RET по завершені фонової 
; задачі, а це можливо лише після завершення інших задач
	call retint8
	call retint33
	sti
	mov ax,4c00h
	int 21h
_TEXT ends
	end begin