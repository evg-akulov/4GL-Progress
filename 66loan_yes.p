/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 2013  ����᪨� �� ��� "���ᥫ�宧����" 
     Filename: loan_yes.p
      Comment: ��������� ������஢
   Parameters:
               ipFrmSelChar - ��ப� ����ன��:
                  <����� ��� ����砥�>,!<����� ��� �᪫�砥�>|<�⡮� ���㤭����>
                  <����� ��� ����砥�>    - ���,
                  <�⡮� ���㤭����>       - ����祭�� 䨫��� �� ���㤭���� (1-���뢠�� ���㤭����,0-�᪫���� ���㤭����)

               �ਬ��:
                  455,!458,!459|0

      Created: 26/12/2012
     Modified: 15/01/2013

*/

{globals.i}
{tmprecid.def}
{setdest.i}

DEF INPUT PARAM ipFrmSelChar AS CHAR NO-UNDO. /* ��ப� ����ன��. */

DEF VAR klient      AS CHAR NO-UNDO.
DEF VAR teleph      AS CHAR NO-UNDO.
DEF VAR vTypKl      AS CHAR NO-UNDO.
DEF VAR vDate       AS DATE LABEL "��� ����        " NO-UNDO.
DEF VAR mListParam  AS CHAR NO-UNDO. /*���᮪ ��ࠬ��஢, �������� � ����ன�� ��������*/
DEF VAR mAcct1  AS CHAR NO-UNDO.
DEF VAR mAcct2  AS CHAR NO-UNDO.
DEF VAR mAcct3  AS CHAR NO-UNDO.
DEF VAR mFlSotr  AS CHAR NO-UNDO.

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

mListParam = ENTRY(1, ipFrmSelChar, "|").
mFlSotr    = ENTRY(2, ipFrmSelChar, "|").

mAcct1 = ENTRY(1, mListParam, ",").
mAcct2 = ENTRY(2, mListParam, ",").
mAcct3 = ENTRY(3, mListParam, ",").

mAcct2 = REPLACE ( mAcct2 , "!" , "" ).
mAcct3 = REPLACE ( mAcct3 , "!" , "" ).


put unformatted "��������� ������஢" skip. 
PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED "N �������;��� ������;���;⥫�䮭�;����" SKIP.  

for each tmprecid no-lock, 
     first loan where recid(loan) = tmprecid.id no-lock:
     /* ஫� �।��*/ 
     if can-find(first loan-acct where 
                       loan-acct.acct BEGINS mAcct1
                       and loan-acct.contract = loan.contract
                       and loan-acct.cont-code = loan.cont-code
                       and loan-acct.acct-type = '�।��'
                       and loan-acct.since <= end-date

			   FIND LAST b-acct-pos WHERE 
		             b-acct-pos.acct EQ loan-acct.acct  AND
		             b-acct-pos.since <= end-date
			   NO-LOCK NO-ERROR.
			   IF AVAILABLE b-acct-pos THEN vRest = b-acct-pos.balance.

                       and acct-pos.acct EQ 0 )


     and not can-find(first loan-acct where 
                       loan-acct.acct BEGINS mAcct2
                       and loan-acct.contract = loan.contract
                       and loan-acct.cont-code = loan.cont-code
/*                       and loan-acct.acct-type = '�।��'*/
                       and loan-acct.since <= end-date)
        
     and not can-find(first loan-acct where 
                       loan-acct.acct BEGINS mAcct3
                       and loan-acct.contract = loan.contract
                       and loan-acct.cont-code = loan.cont-code
/*                       and loan-acct.acct-type = '�।��'*/
                       and loan-acct.since <= end-date) 
                       then do: 

                        find person where person.person-id = loan.cust-id no-lock no-error.
                           if available person then DO:

                             IF INT64(mFlSotr) = 0 THEN DO:
                             IF not GetTempXAttrValueEx ("person", STRING(person.person-id), "�������", vDate, "") EQ "����㤭" then do:
                               assign klient = string(person.person-id) + ";" + person.name-last + " " + person.first-names.
                               teleph = "���." + trim(person.phone[1]) + " ���.,���. " + trim(person.phone[2]).
/*                               vTypKl = GetTempXAttrValueEx ("person", STRING(person.person-id), "�������", vDate, "").*/
                               PUT UNFORMATTED loan.cont-code ";" STRING(klient,"x(40)") ";" teleph ";" person.address SKIP.    
                             END.
                             END. 

                             IF INT64(mFlSotr) = 1 then DO:
                               assign klient = string(person.person-id) + ";" + person.name-last + " " + person.first-names.
                               teleph = "���." + trim(person.phone[1]) + " ���.,���. " + trim(person.phone[2]).
                               PUT UNFORMATTED loan.cont-code ";" STRING(klient,"x(40)") ";" teleph ";" person.address SKIP.    
                             END.

                           end.

     end. 

end.

PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).


{preview.i}