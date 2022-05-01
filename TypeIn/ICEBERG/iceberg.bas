   10 print"iceberg"
   15 g=16
   20 dim b(g,g)
   30 n=int(rnd(1)*g+4)
   40 for i=1 to n
   50 b(int(rnd(1)*g+1),int(rnd(1)*g+1))=42
   60 next
   70 sx=int(rnd(1)*g+1)
   80 sy=int(rnd(1)*g+1)
   90 if b(sx,sy)<>0 then 70
  100 b(sx,sy)=90
  110 yx=int(rnd(1)*g+1)
  120 yy=int(rnd(1)*g+1)
  130 if b(yx,yy)<>0 then 110
  140 b(yx,yy)=89
  150 print"{clr}"
  160 for y=1 to g
  170 for x=1 to g
  180 if b(x,y)=0 then 210
  190 print chr$(b(x,y));
  200 goto 220
  210 print".";
  220 print" ";
  230 next x
  240 print
  250 next y
  260 b(yx,yy)=0
  270 print"direction (n,s,e,w) ";
  280 input d$
  290 yy=yy-(d$="s" and yy<>g)
  300 yy=yy+(d$="n" and yy<>1)
  310 yx=yx-(d$="e" and yx<>g)
  320 yx=yx+(d$="w" and yx<>1)
  330 if b(yx,yy)=90 then 500
  340 if b(yx,yy)=42 then 600
  350 b(yx,yy)=89
  360 b(sx,sy)=0
  370 sx=sx+sgn(yx-sx)
  380 sy=sy+sgn(yy-sy)
  390 if b(sx,sy)=89 then 500
  400 if b(sx,sy)=42 then 700
  410 b(sx,sy)=90
  420 goto 150
  500 print"you've been caught"
  510 goto 800
  600 print"you've hit an iceberg"
  610 goto 800
  700 print"you're safe - he's hit one"
  800 stop
