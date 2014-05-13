/*
   ����᪨� �� ��� "���ᥫ�宧����"
   ��ਫ��� �.�. 
*/

{globals.i}
{49base.fun}
{49loan.fun}
{tmprecid.def}
{setdest.i}
{sh-defs.i}

DEF INPUT PARAM ipFrmSelChar AS CHAR NO-UNDO. /* ��ப� ����ன��. */

DEF VAR vDate       AS DATE LABEL "��� ����        " NO-UNDO.
DEF VAR vSum1       AS DECIMAL NO-UNDO.
DEF VAR vSum2       AS DECIMAL NO-UNDO.
DEF VAR mAcct1      AS CHAR NO-UNDO.
DEF VAR vTurnCr1    AS DECIMAL NO-UNDO.

vDate = gend-date.

/* �஢�ઠ �室��� ��ࠬ��஢. */
IF ipFrmSelChar EQ ""
THEN DO:
   MESSAGE
      COLOR messages
      "�� 㪠���� ��ࠬ���� ��������."
   VIEW-AS ALERT-BOX.

   RETURN.
END.

put unformatted "��������� ������஢ � ����祭��� �������������� / ��業⠬� " skip(2).

for each tmprecid,
    each loan where recid(loan) = tmprecid.id
                and loan.close-date eq ?
                no-lock:

  /*10 - ����祭��� �������������*/
  vSum1 = GetRestByRole(loan.contract, loan.cont-code, "�।��", vDate).
 
  /*11 ����祭�� ��業��*/
  vSum2 = GetRestByRole(loan.contract, loan.cont-code, "�।��%", vDate) +
         GetRestByRole(loan.contract, loan.cont-code, "�।��%�", vDate).


  IF vSum1 <> 0 OR vSum2 <> 0 THEN DO:
/*    PUT UNFORMATTED "������� ����窨" SKIP.*/
    PUT UNFORMATTED loan.cont-code ";" loan.cust-cat ";" loan.cust-id ";" GetClientNameKF(loan.cust-cat, loan.cust-id) ";" GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"���ண���", ?) ";" vSum1 ";" vSum2 SKIP.
/*    PUT UNFORMATTED "���⪨ �� ����:" SKIP.*/
  /*  ᬮ�ਬ ��� ������ 
            mAcct1 = "".
            vTurnCr1 = 0.00.		
	    FOR EACH acct WHERE acct.cust-cat = loan.cust-cat 
                       AND acct.cust-id = loan.cust-id 
                       and acct.bal-acct = 40817
                       and acct.close-date = ?
	                    NO-LOCK:
	       mAcct1 = acct.acct.
               RUN acct-pos in h_base (acct.acct, acct.currency, vDate, vDate, ?).
	       vTurnCr1 = ABS(sh-bal).
	       IF ipFrmSelChar = "1" THEN DO:
		       PUT UNFORMATTED mAcct1 " ���⮪: " vTurnCr1 SKIP.
	       END.
	       IF ipFrmSelChar = "2" AND vTurnCr1 <> 0 THEN DO:
		       PUT UNFORMATTED mAcct1 " ���⮪: " vTurnCr1 SKIP.
	       END.
            END.
	    FOR EACH acct WHERE acct.cust-cat = loan.cust-cat 
                       AND acct.cust-id = loan.cust-id */
/*  �� ��墠�뢠��     and CAN-DO("423",acct.acct)
                       and (acct.bal-acct = 42301 or acct.bal-acct = 42303 or acct.bal-acct = 42304 or acct.bal-acct = 42305 or acct.bal-acct = 42306 or acct.bal-acct = 42307)
                       and acct.close-date = ?
	                    NO-LOCK:
	       mAcct1 = acct.acct.
               RUN acct-pos in h_base (acct.acct, acct.currency, vDate, vDate, ?).
	       vTurnCr1 = ABS(sh-bal).
	       IF ipFrmSelChar = "1" THEN DO:
		       PUT UNFORMATTED mAcct1 " ���⮪: " vTurnCr1 SKIP.
	       END.
	       IF ipFrmSelChar = "2" AND vTurnCr1 <> 0 THEN DO:
		       PUT UNFORMATTED mAcct1 " ���⮪: " vTurnCr1 SKIP.
	       END.
            END.
    PUT UNFORMATTED "=================================================================" SKIP.*/
  END.

END. 

PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}


