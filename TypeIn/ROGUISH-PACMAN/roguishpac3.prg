0
 � ROGUISH V0.3 (PACMAN EDITION) - APR 2022 @ � BY SPIRO  j � FOR THE NOT A WIZARD GAME CHALLENGE � � 53280,0:�53281,0 � X��(�TI) � PL�64:EN�38:WL�35:DR�122 � BX�38:BY�16: � BOARD X,Y SIZE � � B(BX,BY) � � T1:� T2 � � T3:� T4 	 � SET UP BOARD .	  �"�                ROGUISH":� H	" � Y�1 � BY:� X�1 � BX P	$ � D e	& � D�0 � B(X,Y)�0 {	( � D�1 � B(X,Y)�WL �	* �:� �	, � PLOT DOOR �	. � 1000 �	0 DX�ZX:DY�ZY:B(DX,DY)�DR �	2 � ENEMY1 START LOC �	4 � 1000 �	6 X1�ZX:Y1�ZY : B(X1,Y1)�EN 
8 � E1 
: � ENEMY 2 START LOC &
< � 1000 D
> X2�ZX:Y2�ZY : B(X2,Y2)�EN `
@ � PLAYER START LOCATION k
B � 1000 �
D PX�ZX:PY�ZY : B(PX,PY)�EN �
d � PRINT OUT BOARD �
f � Y�1 � BY �
h � " "; �
j � X�1 � BX   �
l � B(X,Y)�WL � � "�"�(B(X,Y));:� 150 �
n � "."; � � X � � � � Y #� B(PX,PY)�0 [� M$�"REACH THE EXIT ("��(DR)�") FOR FREEDOM":� 1420 x� � PLACE OBJECTS ON BOARD �� � 1200:� 1210:� 1220:� 1230 �� � ************************  MAIN LOOP  **************** �� � D$:� D$�"" � 200 �� ZC$�"." � B(PX,PY)�0 � ZX�PX:ZY�PY:� 1400 5� ZX�X1:ZY�Y1:� 1400 L� ZX�X2:ZY�Y2:� 1400 g� � UPDATE PLAYER COORDS �� � D$��(157) � PX�T1 � PY�T2 � PX�T3:PY�T4:� 400 �� � D$��(29) � PX�T3 � PY�T4 � PX�T1:PY�T2:� 400 �� PY�PY�(D$��(17) � PY��BY � B(PX,PY�1)��WL) ,� PY�PY�(D$��(145) � PY��1 � B(PX,PY�1)��WL) [� PX�PX�(D$��(29) � PX��BX � B(PX�1,PY)��WL) �� PX�PX�(D$��(157) � PX��1 � B(PX�1,PY)��WL) ��� B(PX,PY)�DR � 1520 : � PLAYER SAFE, REACHED DOOR ��� B(PX,PY)�EN � 1500 : � PLAYER WALKED INTO ENEMY �B(PX,PY)�PL �B(X1,Y1)�0 %�B(X2,Y2)�0 @�� UPDATE ENEMY1 COORDS S�NX�X1��(PX�X1) f�NY�Y1��(PY�Y1) ��� B(NX,NY)�0 � B(NX,NY)�PL � X1�NX:Y1�NY:� 520 ��� B(X1,NY)�0 � B(NX,NY)�PL � Y1�NY:� 520 ��� B(NX,Y1)�0 � B(NX,NY)�PL � X1�NX:� 520 � UPDATE ENEMY2 COORDS 5
� B(X1,Y1)�PL � ZX�X1:ZY�Y1:� 1500 EB(X1,Y1)�EN XNX�X2��(PX�X2) kNY�Y2��(PY�Y2) �� B(NX,NY)�0 � B(NX,NY)�PL � X2�NX:Y2�NY:� 540 �� B(X2,NY)�0 � B(NX,NY)�PL � Y2�NY:� 540 �� B(NX,Y2)�0 � B(NX,NY)�PL � X2�NX:� 540 � UPDATE ENEMY 3 COORDS ;� B(X2,Y2)�PL � ZX�X2:ZY�Y2:� 1500 K B(X2,Y2)�EN fX� GOT NEW ENEMY COORDS qZ� 1210 |\� 1220 �^� 1230 �`� 200 �b� ***************** FUNCTION SECTION ******** ��� COORD GENERATOR ��ZX��(�(1)�BX�1):ZY��(�(1)�BY�1) �� B(ZX,ZY)��0 � 1000 �� 6�� PLACE DOOR ON BOARD M�ZC$�"�"��(DR)�"�" d�ZY�DY:ZX�DX:� 1400 j�� ��� PLACE PLAYER ON BOARD ��ZC$�"�"��(PL) ��ZY�PY:ZX�PX:� 1400 ��� ��� PLACE ENEMY 1 ON BOARD ��ZC$�""��(EN) ��ZY�Y1:ZX�X1:� 1400 �� �� PLACE ENEMY 2 ON BOARD 0�ZC$�"�"��(EN) G�ZX�X2:ZY�Y2:� 1400 M�� `x� UPDATE BOARD jz� "" �|� Y�1 � ZY : � ""; :� �~� �ZX)ZC$; ��� ��� PRINT MESSAGE ���"":� Y�1 � BY�3�ZS:� "";:� ��� "�"M$ ��� �� ************ END GAME SCENARIOS ********** !�� CAUGHT! 3�� 1220:� 1230 J�ZC$�""��(EN)�"�" U�� 1400 ��M$�"YOU'VE BEEN CAUGHT BY THE GOBLIN!":� 1420 ��� 1800 ��� ESCAPED! ��ZC$�"�"��(PL)�"�" ��ZX�DX:ZY�DY:� 1400 ��� 1210 �M$�"YOU'VE REACHED THE EXIT AND ESCAPED!":� 1420 IZS�1:M$�"�WOULD YOU LIKE TO PLAY AGAIN? (Y/N)":� 1420 a� K$:� K$�"" � 1810 r� K$�"Y" � � x� �� LEVEL ONE MAP �� D� 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 6� D� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 �� D� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 � � D� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 5"� D� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �$� D� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �&� D� 1,0,0,0,0,0,0,0,1,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 4(� D� 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1 �*� D� 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 �,� D� 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 3.� D� 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 �0� D� 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �2� D� 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 24� D� 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �6� D� 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �8� D� 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 �:� PACMAN MAP �<� 1,8,38,8 N>� 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 �@� 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �B� 1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,1 DD� 1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1 �F� 1,0,1,1,0,1,1,1,1,1,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,1,1,1,1,0,1,1,0,1 �H� 1,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,1,1,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,1 :J� 1,1,1,1,0,0,1,0,0,1,0,1,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,0,1,0,0,1,0,0,1,1,1,1 �L� 0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,1,1,1,1,1,1,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,0 �N� 1,1,1,1,0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,1,1,1 0P� 1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1 �R� 1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1 �T� 1,1,0,1,0,0,0,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,0,1,1 &V� 1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1 xX� 1,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,0,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,1 �Z� 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 \� 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 >^� EAST EXIT=B(1,8) -> B(38,8)   