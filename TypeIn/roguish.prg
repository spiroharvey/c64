
 � ROGUISH V0.1 - APR 2022 / � BY SPIRO  Y � FOR THE NOT A WIZARD GAME CHALLENGE p � 53280,0:�53281,0 } X��(�TI) � PL�64:EN�38:WL�35:DR�122 � BX�38:BY�16: � BOARD X,Y SIZE � � B(BX,BY) � � SET UP BOARD 	 �"�                ROGUISH":� 	 � Y�1 � BY:� X�1 � BX #	  � D 8	" � D�0 � B(X,Y)�0 N	$ � D�1 � B(X,Y)�WL V	& �:� q	( � ESCAPE DOOR LOCATION �	* DX��(�(1)�BX�1) �	, DY��(�(1)�BY�1) �	. � B(DX,DY)��0 � 40 �	2 � ENEMY START LOCATION �	4 SX��(�(1)�BX�1) �	6 SY��(�(1)�BY�1) 

8 � B(SX,SY)��0 � 50 &
< � PLAYER START LOCATION :
> YX��(�(1)�BX�1) N
@ YY��(�(1)�BY�1) e
B � B(YX,YY)��0 � 60 {
F � PRINT OUT BOARD �
H � Y�1 � BY �
J � " "; �
L � X�1 � BX   �
N � B(X,Y)�0 � � "."; �
P � B(X,Y)�WL � � "�"�(B(X,Y)); �
R � X �
T � �
V � Y X B(YX,YY)�0 =Z M$�"REACH THE EXIT ("��(DR)�") FOR FREEDOM":� 260 X\ � 200 : � PLACE PLAYER r^ � 210 : � PLACE ENEMY �` � 220 : � PLACE DOOR �d � ** MAIN LOOP �f � D$:� D$�"" � 100 �h ZC$�"." �j B(YX,YY)�0 �l ZY�YY:ZX�YX:� 250 �n ZY�SY:ZX�SX:� 250 p � UPDATE PLAYER COORDS Rr YY�YY�((D$�"S" � D$��(17)) � YY��BY � B(YX,YY�1)��WL) �t YY�YY�((D$�"N" � D$��(145)) � YY��1 � B(YX,YY�1)��WL) �v YX�YX�((D$�"E" � D$��(29)) � YX��BX � B(YX�1,YY)��WL)  x YX�YX�((D$�"W" � D$��(157)) � YX��1 � B(YX�1,YY)��WL) 6z � B(YX,YY)�DR � 520 : � PLAYER SAFE, REACHED DOOR k| � B(YX,YY)�EN � 500 : � PLAYER WALKED INTO ENEMY {~ B(YX,YY)�PL �� B(SX,SY)�0 �� � UPDATE ENEMY COORDS �� NX�SX��(YX�SX) �� NY�SY��(YY�SY) �� � B(NX,NY)��WL � SX�NX:SY�NY:� 180 � � B(SX,NY)��WL � SY�NY:� 180 3� � B(NX,SY)��WL � SX�NX:� 180 N� � GOT NEW ENEMY COORDS f� � B(SX,SY)�PL � 500 v� B(SX,SY)�EN �� � 200 �� � 210 �� � 100 �� � PLACE PLAYER ON BOARD �� ZC$�"�"��(PL) �� ZY�YY:ZX�YX:� 250 �� � �� � PLACE ENEMY ON BOARD � ZC$�""��(EN) !� ZY�SY:ZX�SX:� 250 '� � A� � PLACE DOOR ON BOARD X� ZC$�"�"��(DR)�"�" n� ZY�DY:ZX�DX:� 250 ~� B(DX,DY)�DR �� � �� � UPDATE BOARD �� � "" �� � Y�1 � ZY : � ""; :� � � �ZX)ZC$; �� �� PRINT MESSAGE �"":� Y�1 � BY�3�ZS:� "";:� � "�"M$ 
� '�� CAUGHT! >�ZC$�""��(EN)�"�" T�ZX�SX:ZY�SY:� 250 ��M$�"YOU'VE BEEN CAUGHT BY THE GOBLIN!":� 260 ��� 800 �� ESCAPED! �
ZC$�"�"��(PL)�"�" �ZX�DX:ZY�DY:� 250 �� 210 	M$�"YOU'VE REACHED THE EXIT AND ESCAPED!":� 260 B ZS�2:M$�"�WOULD YOU LIKE TO PLAY AGAIN? (Y/N)":� 260 Y*� K$:� K$�"" � 810 j,� K$�"Y" � � p.� �0� LEVEL ONE MAP �2� 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 (4� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 z6� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 �8� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 :� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 p<� 1,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �>� 1,0,0,0,0,0,0,0,1,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 @� 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1 fB� 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 �D� 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 
F� 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1 \H� 1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �J� 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1  L� 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 RN� 1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 �P� 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1   