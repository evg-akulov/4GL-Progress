{globals.i}
{66base.fun}
{wordwrap.def}
{branch-types.def}

DEF INPUT PARAM RID AS RecID NO-UNDO.

DEF VAR vMerchantName    AS CHARACTER            NO-UNDO.
DEF VAR vMerchantAddr    AS CHARACTER            NO-UNDO.
DEF VAR vMerchantINN     AS CHARACTER            NO-UNDO.
DEF VAR vMerchantKPP     AS CHARACTER            NO-UNDO.
DEF VAR vSenderName      AS CHARACTER            NO-UNDO.
DEF VAR vSenderAddr      AS CHARACTER            NO-UNDO.
DEF VAR vNumSF           AS CHARACTER            NO-UNDO.
DEF VAR vPayeeDocNum     AS CHARACTER            NO-UNDO.
DEF VAR vPayeeDocDate    AS CHARACTER            NO-UNDO.
DEF VAR vCustomerName    AS CHARACTER            NO-UNDO.
DEF VAR vCustomerAddr    AS CHARACTER            NO-UNDO.
DEF VAR vCustomerINN     AS CHARACTER            NO-UNDO.
DEF VAR vCustomerKPP     AS CHARACTER            NO-UNDO.
/*Def Var CEO              As Character            No-Undo.*/
/*Def Var CFO              As Character            No-Undo.*/


DEF VAR vTName           AS CHARACTER EXTENT  10 NO-UNDO.
DEF VAR vUnit            AS CHARACTER            NO-UNDO.
DEF VAR vCount           AS INTEGER              NO-UNDO.
DEF VAR vCounter         AS INTEGER              NO-UNDO.
DEF VAR vPrice           AS DECIMAL              NO-UNDO.
DEF VAR vCost            AS DECIMAL              NO-UNDO.
DEF VAR vRateNDS         AS DECIMAL              NO-UNDO.
DEF VAR vSumNDS          AS DECIMAL              NO-UNDO.
DEF VAR vCostAll         AS DECIMAL              NO-UNDO.
DEF VAR vDirKO           AS CHARACTER            NO-UNDO.
DEF VAR vGBKO            AS CHARACTER            NO-UNDO.

DEF VAR vAccDt           AS CHARACTER            NO-UNDO.
DEF VAR vOp              AS INTEGER              NO-UNDO.
DEF VAR vNTrans          AS INTEGER              NO-UNDO.
DEF VAR vDocDate         AS DATE                 NO-UNDO.
DEF VAR vUserId          AS CHARACTER            NO-UNDO.
DEF VAR vOpKind          AS CHARACTER            NO-UNDO.

DEF VAR vTmpStr          AS Character            NO-UNDO.
DEF VAR vStrTemp         AS CHARACTER            NO-UNDO.
DEFINE BUFFER xbranch FOR branch.
/*DEFINE VAR ts AS CHAR NO-UNDO.  */


FUNCTION Date2StrR RETURNS CHAR
   (iDate AS DATE):

   DEF VAR vMonName    AS CHAR     NO-UNDO EXTENT 12 INITIAL
   [ "ﭢ���" , "䥢ࠫ�", "����"   ,
     "��५�" , "���"    , "���"    ,
     "���"   , "������", "ᥭ����",
     "������", "�����" , "�������"
   ].
   DEF VAR vStr   AS CHAR NO-UNDO.

   vStr = STRING(DAY(iDate), "99") + " " +
          vMonName[ MONTH(iDate) ] + " " +
          STRING(YEAR(iDate), "9999") + " �."
   NO-ERROR.
   RETURN vStr.
END FUNCTION.


/*���*/
FOR FIRST op WHERE RecID(op) = RID NO-LOCK,
FIRST op-entry OF op
NO-LOCK:
 vMerchantName = "��� ""���ᥫ�宧����""".
 vMerchantAddr = "����ਭ᪨� ���., �.3, �.��᪢�, 119034".
 vMerchantINN = TRIM(FGetSetting("���",?,"")).
 vMerchantKPP = TRIM(FGetSetting("�������",?,"")).
 vSenderName = TRIM(FGetSetting("����",?,"")).
 vSenderAddr = TRIM(FGetSetting("����_��",?,"")).
 FOR FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK:
   IF acct.cust-cat = "�" THEN
   DO:
    FIND FIRST cust-corp WHERE
               cust-corp.cust-id = acct.cust-id
    NO-LOCK NO-ERROR.
    IF AVAILABLE cust-corp THEN
     DO:
      CASE cust-corp.cust-id:
       WHEN 227  THEN vCustomerName = "��� ""��ଠ ���࣮����""".
       WHEN 2444 THEN vCustomerName = TRIM(cust-corp.name-short).
       OTHERWISE vCustomerName = TRIM(cust-corp.cust-stat) + " " + TRIM(cust-corp.name-corp).
      END CASE.
      vCustomerINN = TRIM(cust-corp.inn).
      vCustomerKPP = TRIM(GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "���")).
      vCustomerAddr = GetClientAddressKF2("�", cust-corp.cust-id, "�����").
      IF vCustomerAddr EQ "" THEN
       vCustomerAddr = GetClientAddressKF2("�", cust-corp.cust-id, "�������").
      IF vCustomerAddr EQ "" THEN
       vCustomerAddr = GetClientAddressKF2("�", cust-corp.cust-id, "�������").
      IF cust-corp.cust-id EQ 3924 THEN
      DO:
       vCustomerName = "�����⮥ ��樮��୮� ����⢮ ""����⠫�""".
       vCustomerAddr = "660093, ��᭮��᪨� �ࠩ, �.��᭮���, �.�����ୠ�, 263".
       vCustomerINN = "2464228048".
       vCustomerKPP = "245043001".
      END.
     END.
   END.
   IF CAN-DO("47423........6*,47423........7*,47423........8*", acct.acct) AND (acct.cust-cat = "�") THEN
   DO:
     vCustomerName = "�����᪮� ���".
     vCustomerINN = "-".
     vCustomerKPP = "-".
     vCustomerAddr = "-".
   END.
   IF (NOT acct.acct BEGINS "47423") AND (acct.cust-cat = "�") THEN
   DO:
    vCustomerName = "��� ""���ᥫ�宧����""".
    vCustomerINN = TRIM(FGetSetting("���",?,"")).
    vCustomerKPP = TRIM(FGetSetting("�������",?,"")).
    vCustomerAddr = "���.����ਭ᪨�, �.3, �.��᪢�, 119034".
   END.
 END.
 vCount = INTEGER(GetXAttrValue("op", STRING(op.op), "���-��")).
 vRateNDS = DECIMAL(GetXAttrValue("op", STRING(op.op), "�⠢��")).
 IF vRateNDS EQ 0 THEN
  vRateNDS = 18.
 vNumSF = TRIM(GetXAttrValue("op", STRING(op.op), "�������")).
 vSumNDS = op-entry.amt-rub.
 vOp = op.op.
 vPayeeDocDate = Date2StrR(op.op-date).
 vUserId = op.user-id.
 vOpKind = op.op-kind.
 vNTrans = op.op-transaction.
 vAccDt = op-entry.acct-db.
 vDocDate = op.op-date.
END.


/*  ���᫥��� ���祭�� ᯥ樠�쭮�� ���� CEO */
/*ts = "".
vTmpStr = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","6600").
IF {assigned vTmpStr} THEN DO:
   FIND FIRST xbranch WHERE xbranch.branch-id = vTmpStr NO-LOCK NO-ERROR.
   IF AVAIL xbranch
      THEN vTmpStr = xbranch.mgr-name.
      ELSE vTmpStr = "".
END.

CEO = ts + "" + (IF {assigned vTmpStr}
                               THEN vTmpStr
                               ELSE "")
.*/
/* ���᫥��� ���祭�� ᯥ樠�쭮�� ���� CFO */
/*ts = "������ ��壠���".
vTmpStr = GetXAttrValueEx("_user",USERID("bisquit"),"�⤥�����","6600").
IF {assigned vTmpStr} THEN DO:
   FIND FIRST xbranch WHERE xbranch.branch-id = vTmpStr NO-LOCK NO-ERROR.
   IF AVAIL xbranch THEN
      vTmpStr = xbranch.cfo-name.
   ELSE vTmpStr = "".
END.
 
CFO = ts + " _________________________ " + (IF {assigned vTmpStr}
                               THEN vTmpStr
                               ELSE "").*/



vPayeeDocNum = "-".
FOR FIRST op WHERE op.op <> vOp AND
                   op.op-date  EQ vDocDate AND
                   op.op-transaction EQ vNTrans AND
                   CAN-DO("01,06o",op.doc-type)
NO-LOCK,
FIRST op-entry OF op WHERE 
      (op.doc-type EQ "01" AND op-entry.acct-db EQ vAccDt) OR
      (op.doc-type EQ "06o" AND op-entry.acct-cr EQ vAccDt)
NO-LOCK:
 vPayeeDocNum = op.doc-num.
END.

/*������� ��� ���� �� �७��*/
FOR FIRST op WHERE op.op <> vOp AND
                   op.op-date  EQ vDocDate AND
                   CAN-DO("01,06",op.doc-type) AND
                   ((op.op-transaction EQ vNTrans) OR
                    ((vOpKind EQ "i-rshb") AND
                     (op.op-kind EQ "i-rshb") AND                            
                     (op.user-id EQ vUserId) AND
                     ((INTEGER(op.doc-num) + 1) EQ INTEGER(vPayeeDocNum))))
NO-LOCK,
FIRST op-entry OF op WHERE 
      op-entry.acct-db EQ vAccDt
NO-LOCK:
 vTName[1] = op.details.
 vCost = op-entry.amt-rub.
 vCostAll = vCost + vSumNDS.
 vPrice = IF vCount NE 0 THEN vCost / vCount ELSE 0.
 vUnit = "��.".
END.

vTName[1] = ENTRY(1, vTName[1], "~n").
vTName[1] = REPLACE(vTName[1],"���������� ��������","��������").
vTName[1] = REPLACE(vTName[1],"��������� ��������","��������").

{wordwrap.i &s=vTName &l=35 &n=10}

IF vAccDt BEGINS "60311" THEN
  PUT UNFORMATTED "�७�� ���㤠��⢥����� (�㭨樯��쭮��) �����⢠" SKIP(1).
  PUT UNFORMATTED "                                                                                                                                                                " SKIP.
  PUT UNFORMATTED "                                                                                                 �ਫ������ 3                                                   " SKIP.
  PUT UNFORMATTED "                                                                                                 � ��������� � ���浪� ���᫥��� � 㯫��� ������               " SKIP.
  PUT UNFORMATTED "                                                                                                 �� ����������� �⮨����� �66-�                                 " SKIP.
  PUT UNFORMATTED "                                                                                                 (�ਪ�� ��� ���ᥫ�宧���� �� 07.02.2011 �38-��)               " SKIP.
  PUT UNFORMATTED "                                                                                                 (� ।��樨 �ਪ��� ��� ���ᥫ�宧���� �� 20.04.2012 �203-��)  " SKIP(1).
  PUT UNFORMATTED FILL(" ",50) "����-������� � 6600/" vNumSF " �� " vPayeeDocDate FILL(" ",40) "�ਫ������ �1                     " SKIP.
  PUT UNFORMATTED FILL(" ",50) "����������� �       ��" FILL(" ",54) "� ���⠭������� �ࠢ�⥫��⢠     " SKIP.
  PUT UNFORMATTED "                                                                                                                              ���ᨩ᪮� �����樨              " SKIP.                               
  PUT UNFORMATTED "                                                                                                                              �� 26.12.2011 � 1137              " SKIP.
/*PUT UNFORMATTED FILL(" ",50) "����-������� � 6600/" vNumSF " �� " vPayeeDocDate SKIP.*/
/*PUT UNFORMATTED FILL(" ",50) "����������� �       ��" SKIP(2).*/
PUT UNFORMATTED "�த���� " vMerchantName SKIP.
PUT UNFORMATTED "���� " vMerchantAddr SKIP.
PUT UNFORMATTED "���/��� �த��� " vMerchantINN "/" vMerchantKPP SKIP.
PUT UNFORMATTED "��㧮��ࠢ�⥫� � ��� ���� " /*vSenderName ", " vSenderAddr*/ "-" SKIP.
IF vCustomerAddr NE "-" THEN
 vStrTemp = vCustomerName + ", " + vCustomerAddr.
ELSE
 vStrTemp = "-".
PUT UNFORMATTED "��㧮�����⥫� � ��� ���� " /*vStrTemp*/ "-" SKIP.
IF vPayeeDocNum NE "-" THEN
 PUT UNFORMATTED "� ���⥦���� ���㬥��� � " vPayeeDocNum " �� " vPayeeDocDate SKIP.
ELSE
 PUT UNFORMATTED "� ���⥦���� ���㬥��� � -" SKIP.
PUT UNFORMATTED "���㯠⥫� " vCustomerName SKIP.
PUT UNFORMATTED "���� " vCustomerAddr SKIP.
IF vCustomerINN NE "-" THEN
 vStrTemp = vCustomerINN + "/" + vCustomerKPP.
ELSE
 vStrTemp = "-".
PUT UNFORMATTED "���/��� ���㯠⥫� " vStrTemp SKIP.
PUT UNFORMATTED "�����: ������������, ���  ���ᨩ᪨� �㡫�, 643" SKIP(1).

PUT UNFORMATTED "�������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" SKIP.
PUT UNFORMATTED "�                                   �  ������  �����-  ����� (���)�  �⮨�����   �  � ⮬   �����-  �    �㬬�     ��⮨����� ⮢�஢�     ��࠭�     �  �����   �" SKIP.
PUT UNFORMATTED "�        ������������ ⮢��        � ����७�� ���⢮ � �� ������� �   ⮢�஢    �  �᫥   ������  �    ������,   � (ࠡ��, ���), �  �ந�宦����� �⠬��������" SKIP.
PUT UNFORMATTED "�       (���ᠭ�� �믮�������       �����������Ĵ(��ꥬ)�  ����७�� �(ࠡ��,���),�  �㬬�   ��⠢�� ��।�塞�� � �����⢥����   �   ⮢��       �������樨�" SKIP.
PUT UNFORMATTED "�       ࠡ��, ��������� ���),    ����᫮���� �       �            � �����⢥����  ��樧�  �       �  ���㯠⥫�  ��ࠢ � ������� - ����������������Ĵ          �" SKIP.
PUT UNFORMATTED "�        �����⢥����� �ࠢ�       ����������-�       �            � �ࠢ ���     �          �       �              �     �ᥣ�       ���஢�����⪮��          �" SKIP.
PUT UNFORMATTED "�                                   ������(���-�       �            ������� - �ᥣ��          �       �              �                 �  ���   ������- �          �" SKIP.
PUT UNFORMATTED "�                                   � �����쭮�)�       �            �              �          �       �              �                 �        ���������          �" SKIP.
PUT UNFORMATTED "�������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.
PUT UNFORMATTED "�                1                  �2�   2�    �   3   �     4      �      5       �     6    �   7   �      8       �        9        �   10   �  10�  �    11    �" SKIP.
PUT UNFORMATTED "�������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" SKIP.

PUT UNFORMATTED "�" vTName[1] FORMAT "x(35)"        "�-�    -    �   -   �     -      �" 
                                                                      vCost FORMAT ">>>>>>>>>>9.99" "���� ��樧��"
                                                                                   vRateNDS FORMAT ">>>>>9%" "�"
                                                                                            vSumNDS FORMAT ">>>>>>>>>>9.99" "�"
                                                                                                                    vCostAll FORMAT ">>>>>>>>>>>>>9.99" "�   -    �   -   �    -     �" SKIP.

DO vCounter = 2 TO 10:
 IF vTName[vCounter] NE "" THEN
  PUT UNFORMATTED 
                "�" vTName[vCounter] FORMAT "x(35)" "� �         �       �            �              �          �       �              �                 �        �       �          �" SKIP.
END.

PUT UNFORMATTED "���������������������������������������������������������������������������������������������������������������������������������������������������������������������" SKIP.
PUT UNFORMATTED "��ᥣ� � �����                                                      �              �         X        �" vSumNDS FORMAT ">>>>>>>>>>9.99" "|" vCostAll FORMAT ">>>>>>>>>>>>>9.99" "�" SKIP.

PUT UNFORMATTED "�����������������������������������������������������������������������������������������������������������������������������������������" SKIP(2).

put unformatted "�㪮����⥫� �࣠����樨                                                                          ������ ��壠���" skip.
put unformatted "��� ���� 㯮�����祭��� ���  ______________________ �᮫�楢� �.�.                               ��� ���� 㯮�����祭��� ���  ______________________ ����祢� �.�." skip(1). 

PUT UNFORMATTED "��������祭��� �ਪ���� � 33 �� 01.02.2013 �.                                                     㯮�����祭��� �ਪ���� � 066/57 �� 04.03.2014" SKIP(1).

PUT UNFORMATTED "(�������㠫�� �।�ਭ���⥫�)  ______________________________________    (४������ ᢨ��⥫��⢠ �        " SKIP.
PUT UNFORMATTED "                                                                             ���㤠��⢥���� ॣ����樨      " SKIP.
PUT UNFORMATTED "                                                                             �������㠫쭮�� �।�ਭ���⥫�) " SKIP.

PUT UNFORMATTED "�ਬ�砭��. ���� ������� - ���㯠⥫�, ��ன ������� - �த����." SKIP.
