rem roguish v0.1 - apr 2022
rem by spiro 
rem for the not a wizard game challenge

poke 53280,0:poke53281,0
x=rnd(-ti)
pl=64:en=38:wl=35:dr=122
bx=38:by=16: rem board x,y size
dim b(bx,by)

rem set up board
	print"{clr}{white}                roguish":print

	for y=1 to by:for x=1 to bx
	read d
	if d=0 then b(x,y)=0
	if d=1 then b(x,y)=wl
	next:next

40 rem escape door location
	dx=int(rnd(1)*bx+1)
	dy=int(rnd(1)*by+1)
	if b(dx,dy)<>0 then 40


50 rem enemy start location
	sx=int(rnd(1)*bx+1)
	sy=int(rnd(1)*by+1)
	if b(sx,sy)<>0 then 50

60 rem player start location
	yx=int(rnd(1)*bx+1)
	yy=int(rnd(1)*by+1)
	if b(yx,yy)<>0 then 60

70 rem print out board
	for y=1 to by
	print " ";
	for x=1 to bx  
	if b(x,y)=0 then print "{blue}.";
	if b(x,y)=wl then print "{brown}"chr$(b(x,y));
	next x
	print
	next y
	b(yx,yy)=0
	m$="{white}reach the exit ("+chr$(dr)+") for freedom":gosub 260

	gosub 200 : rem place player
	gosub 210 : rem place enemy
	gosub 220 : rem place door

100 rem ** main loop
	get d$:if d$="" then 100

	zc$="{blue}."
	b(yx,yy)=0
	zy=yy:zx=yx:gosub 250
	zy=sy:zx=sx:gosub 250

	rem update player coords
	yy=yy-((d$="s" or d$=chr$(17)) and yy<>by and b(yx,yy+1)<>wl)
	yy=yy+((d$="n" or d$=chr$(145)) and yy<>1 and b(yx,yy-1)<>wl)
	yx=yx-((d$="e" or d$=chr$(29)) and yx<>bx and b(yx+1,yy)<>wl)
	yx=yx+((d$="w" or d$=chr$(157)) and yx<>1 and b(yx-1,yy)<>wl)

	if b(yx,yy)=dr then 520 : rem player safe, reached door
	if b(yx,yy)=en then 500 : rem player walked into enemy

	b(yx,yy)=pl
	b(sx,sy)=0

	rem update enemy coords
	nx=sx+sgn(yx-sx)
	ny=sy+sgn(yy-sy)

	if b(nx,ny)<>wl then sx=nx:sy=ny:goto 180
	if b(sx,ny)<>wl then sy=ny:goto 180
	if b(nx,sy)<>wl then sx=nx:goto 180

180 rem got new enemy coords
	if b(sx,sy)=pl then 500
	b(sx,sy)=en
	gosub 200
	gosub 210

	goto 100


200 rem place player on board
	zc$="{yellow}"+chr$(pl)
	zy=yy:zx=yx:gosub 250
	return

210 rem place enemy on board
	zc$="{green}"+chr$(en)
	zy=sy:zx=sx:gosub 250
	return

220 rem place door on board
	zc$="{RVSON}{yellow}"+chr$(dr)+"{RVSOFF}"
	zy=dy:zx=dx:gosub 250
	b(dx,dy)=dr
	return


250 rem update board
	print "{home}"
	for y=1 to zy : print "{down}"; :next
	print tab(zx)zc$;
	return

260 rem print message
	print"{home}":for y=1 to by+3+zs:print "{down}";:next
	print "{yellow}"m$
	return

500 rem caught!
	zc$="{RVSON}{red}"+chr$(en)+"{RVSOFF}"
	zx=sx:zy=sy:gosub 250
	m$="you've been caught by the goblin!":gosub 260
	goto 800


520 rem escaped!
	zc$="{RVSON}{yellow}"+chr$(pl)+"{RVSOFF}"
	zx=dx:zy=dy:gosub 250
	gosub 210
	m$="you've reached the exit and escaped!":gosub 260

800 zs=2:m$="{yellow}would you like to play again? (y/n)":gosub 260
810 get k$:if k$="" then 810
if k$="y" then run
end

rem level one map

dA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,1,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1
dA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
dA 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

