/*               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2009 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: swordn2.p
      Comment: ����� ᢮����� ����ਠ�쭮�� �थ� � �㡫��.
               ������砥��� � CTRL-G � ���-�� ���.
   Parameters:
         Uses: 
      Used by:
      Created: 17/11/2009 MUTA 0117598: ��.���⭠� �ଠ �������� ����ਠ�쭮�� �थ� �� ���㬥�⠬ ��� (� �㡫�� ��)
     Modified: 
*/

&GLOB rshb              YES
&GLOB FILE_SWORD_P      YES
&GLOB FILE_SWORD_I_RUB  YES
&GLOB SORT-BY           BY SUBSTRING(op-entry.acct-db, 17, 2) BY op-entry.amt-rub

{66sword.i}
