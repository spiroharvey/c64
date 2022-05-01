    5 poke 53280,0:poke53281,0
10 x=rnd(-ti)
   15 g=16
   20 dim b(g,g)

24 rem set up board
25 print"{clr}{white}                iceberg":print

29 rem icebergs
   30 n=int(rnd(1)*g+4)
   40 for i=1 to n
   50 b(int(rnd(1)*g+1),int(rnd(1)*g+1))=42
   60 next

69 rem enemy
   70 sx=int(rnd(1)*g+1)
   80 sy=int(rnd(1)*g+1)
   90 if b(sx,sy)<>0 then 70
  100 b(sx,sy)=90

109 rem player
  110 yx=int(rnd(1)*g+1)
  120 yy=int(rnd(1)*g+1)
  130 if b(yx,yy)<>0 then 110
  140 b(yx,yy)=89



159 rem print out board
  160 for y=1 to g
  170 for x=1 to g
  
180 if b(x,y)=0 then print "{blue}.";
182 if b(x,y)=42 then print "{cyn}"chr$(b(x,y));
184 if b(x,y)=89 then print "{yel}"chr$(b(x,y));
186 if b(x,y)=90 then print "{lred}"chr$(b(x,y));
190 print " ";

  200 next x
  210 print
  220 next y
  230 b(yx,yy)=0
  240 print"{white}direction (n,s,e,w) ";

249 rem ** main loop
250 get d$:if d$="" then 250

252 zc$="{blue}."
254 b(yx,yy)=0
256 zy=yy:zx=yx:gosub 450
257 rem b(sx,sy)=0
258 zy=sy:zx=sx:gosub 450


  290 yy=yy-((d$="s" or d$=chr$(17)) and yy<>g)
  300 yy=yy+((d$="n" or d$=chr$(145)) and yy<>1)
  310 yx=yx-((d$="e" or d$=chr$(29)) and yx<>g)
  320 yx=yx+((d$="w" or d$=chr$(157)) and yx<>1)

  330 if b(yx,yy)=90 then 500
  340 if b(yx,yy)=42 then 600
  350 b(yx,yy)=89
  360 b(sx,sy)=0
  370 sx=sx+sgn(yx-sx)
  380 sy=sy+sgn(yy-sy)
  390 if b(sx,sy)=89 then 500
  400 if b(sx,sy)=42 then 700
  410 b(sx,sy)=90

412 gosub 430
414 gosub 440


  420 goto 250


430 rem display player
431 zc$="{yellow}y"
432 zy=yy:zx=yx:gosub 450
435 return

440 rem display enemy
441 zc$="{lred}z"
442 zy=sy:zx=sx:gosub 450
445 return

450 print "{home}"
451 for y=1 to zy : print "{down}"; :next
452 print tab((zx*2)-2)zc$;
453 return

500 print"{home}":for y=1 to g+3:print "{down}";:next
505 print"{yellow}you've been caught"
510 zc$="{RVSON}{red}z{RVSOFF}"
520 zy=sy:zx=sx:gosub 450
  550 goto 800

600 print"{home}":for y=1 to g+3:print "{down}";:next
605 print"{yellow}you've hit an iceberg"
610 zc$="{RVSON}{red}*{RVSOFF}"
620 zy=yy:zx=yx:gosub 450
625 gosub 440
  650 goto 800

  700 print"{home}":for y=1 to g+3:print "{down}";:next
705 print"{yellow}you're safe - he's hit one"
710 zc$="{RVSON}{red}*{RVSOFF}"
720 zy=sy:zx=sx:gosub 450
725 gosub 430

800 print"{home}":for y=1 to g+5:print "{down}";:next
805 print "{yellow}would you like to play again? (y/n)";
810 get k$:if k$="" then 810
820 if k$="y" then run

