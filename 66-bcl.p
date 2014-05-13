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
{tmprecid.def}
{setdest.i}
{62-Lib.i}

DEF VAR klient       AS CHAR NO-UNDO.
DEF VAR teleph       AS CHAR NO-UNDO.
DEF VAR mProsrV      AS DECIMAL NO-UNDO.


put unformatted "��������� �����⮢" skip. 
PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED "��� ������;���;⥫�䮭�;����;��� ஦�����" SKIP.  


for each tmprecid no-lock, 
    first person where recid(person) = tmprecid.id no-lock:


    if can-find(first acct where 
		       acct.cust-cat = "�" 
                       AND acct.cust-id = person.person-id 
                       and acct.acct BEGINS "455"
                       and acct.contract = '�।��'
                       and acct.close-date = ?)
    then do: 

	    mProsrV = 0.
	    FOR EACH acct WHERE acct.cust-cat = "�" 
	                    AND	acct.cust-id = person.person-id 
	                    AND (acct.bal-acct = 45815 
	                         OR
	                         acct.bal-acct = 45915) 
	                    NO-LOCK:
	       mProsrV = mProsrV + GetOstAcct(acct.acct,end-date).
	    END.

	    IF(mProsrV = 0) THEN DO:
	      IF not GetTempXAttrValueEx ("person", STRING(person.person-id), "�������", end-date, "") EQ "����㤭" then do:
	         assign klient = string(person.person-id) + ";" + person.name-last + " " + person.first-names.
	         teleph = "���." + trim(person.phone[1]) + " ���.,���. " + trim(person.phone[2]).
	         PUT UNFORMATTED STRING(klient,"x(40)") ";" teleph ";" person.address ";" STRING(person.birthday,"99/99/9999") SKIP.    
	      END.
	    END. 

    end.

END.

PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}