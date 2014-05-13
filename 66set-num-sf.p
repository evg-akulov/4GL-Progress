/*
   Красноярский РФ ОАО "Россельхозбанк"
   Кириллов П.Г. 14.03.2011
   
   Нумерация счетов-фактур
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
   vDate     LABEL "      Дата опер.дня" SKIP
   vNumberSF LABEL "Номер счета-фактуры"
WITH WIDTH 40
     CENTERED
     ROW 10
     OVERLAY SIDE-LABELS 0 COL
     TITLE COLOR bright-white "[ ЗАДАЙТЕ ПАРАМЕТРЫ ]".

IF getThisUserXattrValue("Отделение") NE "6600" THEN
DO:
  MESSAGE "Запуск разрешен только сотруднику филиала" VIEW-AS ALERT-BOX.
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
   getThisUserXattrValue("ИзмДопРекв") = "не имеет" THEN
DO:
  vPrevNumberSF = SetCounterValue("SFNumKF",vPrevNumberSF,vDate).
  MESSAGE "Вы не имеете права изменять доп.реквизиты" SKIP
          "в закрытом дне" VIEW-AS ALERT-BOX.
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

  {spinner.i "Обработка..."}

  IF FIRST(acct.branch-id) THEN
  DO:
    PUT UNFORMATTED "Нумерация счетов-фактур в " STRING(vDate,"99.99.9999") SKIP(1).
    PUT UNFORMATTED "Номер      Счет по дебету       Счет по кредиту      Сумма          Старое значение Новое значение " SKIP.
    PUT UNFORMATTED "---------- -------------------- -------------------- -------------- --------------- ---------------" SKIP.
  END.

  vNewNumber = STRING(GetCounterNextValue("SFNumKF",vDate)).
  vOldNumber = GetXAttrValue("op",STRING(op.op),"НомерСФ").
  UpdateSigns("op",string(op.op),"НомерСФ",vNewNumber,NO).
  PUT UNFORMATTED op.doc-num FORMAT "x(10)" " " op-entry.acct-db FORMAT "x(20)" " " op-entry.acct-cr FORMAT "x(20)" " " op-entry.amt-rub FORMAT ">>>>>>>>>>9.99" " " vOldNumber FORMAT "x(15)" " " vNewNumber FORMAT "x(15)" SKIP.
END.

{intrface.del}

{preview.i}
