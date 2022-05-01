rem roguish v0.3 (pacman edition) - apr 2022
rem by spiro 
rem for the not a wizard game challenge

poke 53280,0:poke53281,0
x=rnd(-ti)
pl=64:en=38:wl=35:dr=122
bx=38:by=16: rem board x,y size
dim b(bx,by)
read t1:read t2
read t3:read t4

rem set up board
	print"{clr}{white}                roguish":print

	for y=1 to by:for x=1 to bx
	read d
	if d=0 then b(x,y)=0
	if d=1 then b(x,y)=wl
	next:next

	rem plot door
	gosub 1000
	dx=zx:dy=zy:b(dx,dy)=dr

	rem enemy1 start loc
	gosub 1000
	x1=zx:y1=zy : b(x1,y1)=en
	rem e1

	rem enemy 2 start loc
	gosub 1000
	x2=zx:y2=zy : b(x2,y2)=en

	rem player start location
	gosub 1000
	px=zx:py=zy : b(px,py)=en



100 rem print out board
	for y=1 to by
	print " ";
	for x=1 to bx  
	if b(x,y)=wl then print "{brown}"chr$(b(x,y));:goto 150
	print "{blue}.";
150	next x
	print
	next y
	b(px,py)=0
	m$="{white}reach the exit ("+chr$(dr)+") for freedom":gosub 1420

	rem place objects on board
	gosub 1200:gosub 1210:gosub 1220:gosub 1230

200 rem ************************  main loop  ****************
	get d$:if d$="" then 200

	zc$="{blue}."
	b(px,py)=0
	zx=px:zy=py:gosub 1400
	zx=x1:zy=y1:gosub 1400
	zx=x2:zy=y2:gosub 1400


	rem update player coords
	if d$=chr$(157) and px=t1 and py=t2 then px=t3:py=t4:goto 400
	if d$=chr$(29) and px=t3 and py=t4 then px=t1:py=t2:goto 400

	py=py-(d$=chr$(17) and py<>by and b(px,py+1)<>wl)
	py=py+(d$=chr$(145) and py<>1 and b(px,py-1)<>wl)
	px=px-(d$=chr$(29) and px<>bx and b(px+1,py)<>wl)
	px=px+(d$=chr$(157) and px<>1 and b(px-1,py)<>wl)


400	if b(px,py)=dr then 1520 : rem player safe, reached door
	if b(px,py)=en then 1500 : rem player walked into enemy

	b(px,py)=pl
	b(x1,y1)=0
	b(x2,y2)=0

500	rem update enemy1 coords
	nx=x1+sgn(px-x1)
	ny=y1+sgn(py-y1)

	if b(nx,ny)=0 or b(nx,ny)=pl then x1=nx:y1=ny:goto 520
	if b(x1,ny)=0 or b(nx,ny)=pl then y1=ny:goto 520
	if b(nx,y1)=0 or b(nx,ny)=pl then x1=nx:goto 520

520	rem update enemy2 coords
	if b(x1,y1)=pl then zx=x1:zy=y1:goto 1500
	b(x1,y1)=en

	nx=x2+sgn(px-x2)
	ny=y2+sgn(py-y2)

	if b(nx,ny)=0 or b(nx,ny)=pl then x2=nx:y2=ny:goto 540
	if b(x2,ny)=0 or b(nx,ny)=pl then y2=ny:goto 540
	if b(nx,y2)=0 or b(nx,ny)=pl then x2=nx:goto 540

540 rem update enemy 3 coords
	if b(x2,y2)=pl then zx=x2:zy=y2:goto 1500
	b(x2,y2)=en

600 rem got new enemy coords
	gosub 1210
	gosub 1220
	gosub 1230

	goto 200

rem ***************** function section ********


1000 rem coord generator
	zx=int(rnd(1)*bx+1):zy=int(rnd(1)*by+1)
	if b(zx,zy)<>0 then 1000
	return

1200 rem place door on board
	zc$="{RVSON}{yellow}"+chr$(dr)+"{RVSOFF}"
	zy=dy:zx=dx:gosub 1400
	return

1210 rem place player on board
	zc$="{yellow}"+chr$(pl)
	zy=py:zx=px:gosub 1400
	return

1220 rem place enemy 1 on board
	zc$="{green}"+chr$(en)
	zy=y1:zx=x1:gosub 1400
	return


1230 rem place enemy 2 on board
	zc$="{cyan}"+chr$(en)
	zx=x2:zy=y2:gosub 1400
	return


1400 rem update board
	print "{home}"
	for y=1 to zy : print "{down}"; :next
	print tab(zx)zc$;
	return

1420 rem print message
	print"{home}":for y=1 to by+3+zs:print "{down}";:next
	print "{yellow}"m$
	return

rem ************ end game scenarios **********
1500 rem caught!
	gosub 1220:gosub 1230
	zc$="{RVSON}{red}"+chr$(en)+"{RVSOFF}"
	gosub 1400
	m$="you've been caught by the goblin!":gosub 1420
	goto 1800


1520 rem escaped!
	zc$="{RVSON}{yellow}"+chr$(pl)+"{RVSOFF}"
	zx=dx:zy=dy:gosub 1400
	gosub 1210
	m$="you've reached the exit and escaped!":gosub 1420

1800 zs=1:m$="{yellow}would you like to play again? (y/n)":gosub 1420
1810 get k$:if k$="" then 1810
if k$="y" then run
end


rem level one map
rem dA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
rem dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,1,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1
rem dA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
rem dA 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
rem dA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

rem pacman map
dA 1,8,38,8
dA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
dA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,1
dA 1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,1,1,0,1,1,1,1,1,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,1,1,1,1,0,1,1,0,1
dA 1,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,1,1,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,1
dA 1,1,1,1,0,0,1,0,0,1,0,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,0,1,0,0,1,0,0,1,1,1,1
dA 0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,1,1,1,1,1,1,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,0
dA 1,1,1,1,0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,1,1,1
dA 1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1
dA 1,1,0,1,0,0,0,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,0,1,1
dA 1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1
dA 1,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,0,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,1
dA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

rem east exit=b(1,8) -> b(38,8)
