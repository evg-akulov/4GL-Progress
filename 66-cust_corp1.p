/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� ��������������
      Filename: 
      Comment: 
   Parameters
         Use:
      Used by:
      Created: 24/01/2013
     Modified: 29/01/2013 - ��������� �஢�ઠ 455*
     Modified: 31/01/2013 - �஢�ઠ �� �������� 455*
*/

{globals.i}
{setdest.i}
{tmprecid.def}
{66base.fun}

DEF VAR klient         AS CHARACTER            NO-UNDO.
DEF VAR teleph         AS CHARACTER            NO-UNDO.
DEF VAR vCustomerAddr  AS CHARACTER            NO-UNDO.
DEF VAR ccBranch-id    AS CHARACTER            NO-UNDO.
DEF VAR vTmpStr        AS CHARACTER            NO-UNDO.
DEF VAR vDols          AS CHARACTER            NO-UNDO.
DEF VAR vFIO           AS CHARACTER            NO-UNDO.
DEF VAR vNDog          AS CHARACTER            NO-UNDO.
DEF VAR vIsp           AS CHARACTER            NO-UNDO.

for each tmprecid no-lock, 
    first cust-corp where recid(cust-corp) = tmprecid.id no-lock:

/*80 ��ப*/
PUT UNFORMATTED "                                                                     �ਫ������ 13" SKIP.
PUT UNFORMATTED "                                                                     � �ࠢ���� ������ � ������� � ��� <���ᥫ�宧����>" SKIP.
PUT UNFORMATTED "                                                                     ������᪨� ��⮢ � ����� ���ᨩ᪮� �����樨" SKIP.
PUT UNFORMATTED "                                                                     � �����࠭��� ����� �ਤ��᪨� ��栬, �������㠫�� " SKIP.
PUT UNFORMATTED "                                                                     �।�ਭ���⥫� � 䨧��᪨� ��栬, �������騬�� �" SKIP. 
PUT UNFORMATTED "                                                                     ��⠭�������� ��������⥫��⢮� ���ᨩ᪮� �����樨" SKIP.
PUT UNFORMATTED "                                                                     ���浪� ��⭮� �ࠪ⨪��, � 105-�" SKIP.
PUT UNFORMATTED "                                                                     (�ਪ�� ��� <���ᥫ�宧����> �� 10.05.2007 � 136-��)" SKIP.
PUT UNFORMATTED "                                                                     (� ।��樨 �ਪ���� ��� <���ᥫ�宧����> �� 29.12.2011" SKIP. 
PUT UNFORMATTED "                                                                      � 606-��, �� 31.05.2013 � 268-��)" SKIP(4).

klient        = "".
vCustomerAddr = "".
ccBranch-id   = "".
vTmpStr       = "".
vDols         = "".
vFIO          = "".
vNDog         = "".
vIsp          = "".

       klient = cust-corp.cust-stat + " " + cust-corp.name-corp.
       teleph = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"tel", ?).
       vCustomerAddr = GetClientAddressKF2("�", cust-corp.cust-id, "�����").
       ccBranch-id = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"branch-id", ?).
   FIND FIRST branch WHERE branch.branch-id = ccBranch-id NO-LOCK NO-ERROR.
   IF AVAILABLE branch THEN DO:
    vTmpStr = branch.name.
    vDols = branch.mgr-title.
    vFIO = branch.mgr-name.
   END.

   IF ccBranch-id = "6600" THEN DO:
        vDols = "��砫쭨� ����樮����� �⤥��".
	vFIO  = "�������� �.�.".
   END.
     
   find first acct where    acct.cust-cat = "�" 
                       AND acct.cust-id = cust-corp.cust-id 
                       and (acct.acct BEGINS "40702" OR acct.acct BEGINS "40802")
                       and acct.contract = "�����"
                       and acct.close-date = ? NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN vNDog = GetXattrValueEx("acct",STRING(acct.acct) + "," + STRING(acct.currency),"��������", ?).  

   FIND FIRST _user WHERE _user._userid EQ userid("bisquit") NO-LOCK NO-ERROR.
   IF AVAILABLE _user THEN vIsp = GetXattrValueEx("_user",_user._userid,"_user-name", "_________").


PUT UNFORMATTED "      " vTmpStr SKIP(4).
PUT UNFORMATTED "�����������������������������������������������������������������������������������������������������������������������Ŀ" SKIP.
PUT UNFORMATTED "�                                                      �" klient FORMAT "x(64)"                                        "�" SKIP.
PUT UNFORMATTED "� <____>_________20___�. � __________________________  �                                                                �" SKIP.
PUT UNFORMATTED "�                                                      �" vCustomerAddr FORMAT "x(64)"                                 "�" SKIP.
PUT UNFORMATTED "�������������������������������������������������������������������������������������������������������������������������" SKIP(2).

PUT UNFORMATTED "� �।�⠢����� ���ଠ樨 � ���㬥�⮢," SKIP.
PUT UNFORMATTED "����室���� ��� ���������� ᢥ����� � ������" SKIP(3).

PUT UNFORMATTED "� ᮮ⢥��⢨� � �᫮��ﬨ ������� ������᪮�� ���, " + vNDog + " ࠭�� �����祭���� � ��襩" SKIP.
PUT UNFORMATTED "�࣠����樥�, ���� ����� �ࠢ� �������� ���ࠢ���� ������� ���쬥��� ������ � �।�⠢����� ���ଠ樨 � ���㬥�⮢, ����室���� ��� ����������" SKIP. 
PUT UNFORMATTED "ᢥ����� � ������, �������� � �����, � ������ ��易� �।��⠢���� ⠪�� ���ଠ�� � ���㬥���." SKIP(1).

PUT UNFORMATTED "� �裡 � ��������� ��ᨬ ��� �।��⠢��� � ���� ����室��� ���㬥��� � ���ଠ�� � ���ᥭ�� ��������� � ���������� � ��।�⥫��" SKIP.
PUT UNFORMATTED "���㬥���, ��������� ����, ⥫�䮭�, ८࣠����樨 ��� �������樨 ��襩 �࣠����樨, � ��㣨� ����������, ᯮᮡ��� �������� �� �ᯮ������ �������." SKIP(1).

PUT UNFORMATTED "�।�⠢������ ���� ���ଠ�� �㤥� �ᯮ�짮���� ������ ��� ���������� ᢥ����� � ��襩 �࣠����樨, ᮤ�ঠ���� � ���ଠ樮���� ��⥬�" SKIP.
PUT UNFORMATTED "�����, � ���㬥��� ���� ����饭� � �ਤ��᪮� ���� ��襩 �࣠����樨, ����饥�� � �����." SKIP(4).

PUT UNFORMATTED "   " vDols  format "x(45)"                 " _________________       " vFIO   SKIP.
PUT UNFORMATTED "(��������� 㯮�����祭���� ��� �����/               (�������)           (����஢�� ������)" SKIP.
PUT UNFORMATTED "ॣ�����쭮�� 䨫����/ �������⥫쭮�� ���)" SKIP(1).

PUT UNFORMATTED "��. " + vIsp    SKIP.
PUT UNFORMATTED "���. _________" SKIP(50).

end.

{preview.i}