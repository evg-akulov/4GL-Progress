/*
   ��᭮��᪨� �� ��� "���ᥫ�宧����"
   ��ਫ��� �.�. 14.03.2011
   
   �㬥��� ��⮢-䠪���
*/

{globals.i}
{intrface.get count}
{intrface.get xclass}

DEF VAR vDate         AS DATE NO-UNDO.
DEF VAR vPrevNumberSF AS INTEGER NO-UNDO.
DEF VAR vNumberSF     AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEF VAR vOldNumber    AS CHAR NO-UNDO.
DEF VAR vNewNumber    AS CHAR NO-UNDO.

DEFINE FRAME ParamFrame
   vDate     LABEL "      ��� ����.���" SKIP
   vNumberSF LABEL "����� ���-䠪����"
WITH WIDTH 40
     CENTERED
     ROW 10
     OVERLAY SIDE-LABELS 0 COL
     TITLE COLOR bright-white "[ ������� ��������� ]".

IF getThisUserXattrValue("�⤥�����") NE "6600" THEN
DO:
  MESSAGE "����� ࠧ�襭 ⮫쪮 ���㤭��� 䨫����" VIEW-AS ALERT-BOX.
  RETURN.
END.

vDate = gend-date.
vPrevNumberSF = GetCounterCurrentValue("SFNumKF",vDate).
vNumberSF = GetCounterNextValue("SFNumKF",vDate).
PAUSE 0.
DO TRANSACTION ON ERROR UNDO, LEAVE WITH FRAME ParamFrame:
  UPDATE
    vDate
    vNumberSF
  EDITING:
    READKEY.
    IF LASTKEY EQ KEYCODE("ESC") THEN
     vPrevNumberSF = SetCounterValue("SFNumKF",vPrevNumberSF,vDate).
    APPLY LASTKEY.
  END. 
END.
HIDE FRAME ParamFrame NO-PAUSE.
vNumberSF = SetCounterValue("SFNumKF",vNumberSF - 1,vDate).


IF CAN-FIND(LAST acct-pos WHERE acct-pos.since >= vDate) AND
   getThisUserXattrValue("����������") = "�� �����" THEN
DO:
  vPrevNumberSF = SetCounterValue("SFNumKF",vPrevNumberSF,vDate).
  MESSAGE "�� �� ����� �ࠢ� �������� ���.४������" SKIP
          "� �����⮬ ���" VIEW-AS ALERT-BOX.
  RETURN.
END.


{setdest.i}

FOR EACH op WHERE
         op.op-date    EQ vDate    AND
         op.doc-type   EQ "06"    AND
         op.class-code EQ "opbkfnds"
NO-LOCK,
FIRST op-entry OF op WHERE
      op-entry.acct-cr BEGINS "60309"
NO-LOCK,
FIRST acct WHERE
      acct.acct EQ op-entry.acct-cr
NO-LOCK
  BREAK BY acct.branch-id
        BY op-entry.amt-rub:

  {spinner.i "��ࠡ�⪠..."}

  IF FIRST(acct.branch-id) THEN
  DO:
    PUT UNFORMATTED "�㬥��� ��⮢-䠪��� � " STRING(vDate,"99.99.9999") SKIP(1).
    PUT UNFORMATTED "�����      ��� �� ������       ��� �� �।���      �㬬�          ��஥ ���祭�� ����� ���祭�� " SKIP.
    PUT UNFORMATTED "---------- -------------------- -------------------- -------------- --------------- ---------------" SKIP.
  END.

  vNewNumber = STRING(GetCounterNextValue("SFNumKF",vDate)).
  vOldNumber = GetXAttrValue("op",STRING(op.op),"�������").
  UpdateSigns("op",string(op.op),"�������",vNewNumber,NO).
  PUT UNFORMATTED op.doc-num FORMAT "x(10)" " " op-entry.acct-db FORMAT "x(20)" " " op-entry.acct-cr FORMAT "x(20)" " " op-entry.amt-rub FORMAT ">>>>>>>>>>9.99" " " vOldNumber FORMAT "x(15)" " " vNewNumber FORMAT "x(15)" SKIP.
END.

{intrface.del}

{preview.i}
