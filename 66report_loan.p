/*
   ����᪨� �� ��� "���ᥫ�宧����"
   ��ਫ��� ��堭� 
*/

{globals.i}
{49base.fun}
{49loan.fun}
{intrface.get comm}
{intrface.get i254}
{intrface.get xclass}
{tmprecid.def}
{setdest.i}
{sh-defs.i}

DEF VAR vDate       AS DATE LABEL "��� ����        " NO-UNDO.
DEF VAR vSum1       AS DECIMAL NO-UNDO.
DEF VAR vSum2       AS DECIMAL NO-UNDO.
DEF VAR mAcct1      AS CHAR NO-UNDO.
DEF VAR vTurnCr1    AS DECIMAL NO-UNDO.
DEF VAR Flag        AS CHAR NO-UNDO.
DEF VAR vStr        AS CHAR NO-UNDO.

vDate = gend-date.

put unformatted "��������� �� �ଥ 115 " skip.
put unformatted "N �������       �� �ଠ ���� १   �� �.  ��㤭� ���        ����᪠" skip(2).

for each tmprecid,
    each loan where recid(loan) = tmprecid.id
                and loan.close-date eq ?
                no-lock:
/*�������*/
 PUT UNFORMATTED string(loan.cont-code,"X(15)").
 PUT UNFORMATTED string(loan.risk,"->>,>>9.99").
/*����.*/
  vSum1 = LnRsrvRate(loan.contract, loan.cont-code, vDate).
  IF vSum1 EQ ? THEN 
   vSum1 = 0.
  PUT UNFORMATTED "|����." string(vSum1,">>>9") "|".
 /*�� - �������� !!!*/
  vStr = STRING(LnGetGrRiska(LnRsrvRate(loan.contract, loan.cont-code, vDate), vDate)).
  PUT UNFORMATTED "�� �." vStr "|".
/*���*/
  FIND LAST loan-acct WHERE
            loan-acct.contract  EQ loan.contract AND 
            loan-acct.cont-code EQ loan.cont-code AND
            loan-acct.acct-type EQ "�।��" AND
            loan-acct.since     <= vDate
  NO-LOCK NO-ERROR.
  IF AVAILABLE loan-acct THEN DO:
  	PUT UNFORMATTED loan-acct.acct "|".
  	PUT UNFORMATTED GetXattrValueEx("acct", loan-acct.acct + "," + loan-acct.currency,"����᪠", ?) "|" SKIP.
  END.
END. 

PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}
