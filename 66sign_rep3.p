/*                          
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2004 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: sign_rep2.p
      Comment: ���� "���᮪ ���, ������ �ࠢ� ������"
   Parameters:
      Used by: 
      Created: 22/12/2007 kraw (0082707) 
     Modified: 
*/
{globals.i}             /* �������� ��६���� ��ᨨ. */
{tmprecid.def}

DEFINE VARIABLE mName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStrTMP   AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mClassNm  AS CHARACTER NO-UNDO.
DEFINE VARIABLE teleph 	  AS CHARACTER NO-UNDO.
DEFINE VARIABLE acs 	  AS CHARACTER NO-UNDO.
def var ind1 as int64             NO-UNDO.

{setdest.i}
PUT UNFORMATTED "������� " SKIP. 
PUT UNFORMATTED "���;������������;����;����䮭;���;����;���;����䮭" SKIP. 
FIND FIRST tmprecid NO-ERROR.

IF NOT AVAILABLE tmprecid THEN
   RETURN.

FOR EACH tmprecid,
   EACH cust-corp WHERE RECID(cust-corp) EQ tmprecid.id NO-LOCK:

   PUT UNFORMATTED "              ;"SKIP.
   PUT UNFORMATTED cust-corp.cust-id ";" cust-corp.name-short ";" cust-corp.addr-of-low[1] ";" GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"tel","-") ";" . 

	acs = "".
	FOR EACH acct WHERE  acct.cust-id = cust-corp.cust-id 
			AND acct.acct BEGINS '40' 
			AND acct.cust-cat EQ '�' NO-LOCK:
	   acs = acs + string(acct.acct) + " " .
	END.
	
   PUT UNFORMATTED acs ";" .

/*   PUT UNFORMATTED "              ;"SKIP.
   PUT UNFORMATTED "������������: " cust-corp.name-short ";" SKIP. 
   PUT UNFORMATTED "����: " + cust-corp.addr-of-low[1] ";" SKIP. 
   PUT UNFORMATTED "����䮭: " GetXAttrValueEx("cust-corp",STRING(cust-corp.cust-id),"tel","-") ";" SKIP. 
   PUT UNFORMATTED "���:    ;"SKIP.
	FOR EACH acct WHERE  acct.cust-id = cust-corp.cust-id 
			AND acct.acct BEGINS '40' 
			AND acct.cust-cat EQ '�' NO-LOCK:
	   PUT UNFORMATTED acct.acct ";" SKIP. 
	END.*/

/*   PUT UNFORMATTED " " SKIP. */
	IND1 = 0.
    FOR	EACH cust-role WHERE cust-role.file-name EQ "cust-corp"
                     	AND INT64(cust-role.surrogate) EQ cust-corp.cust-id
			AND (cust-role.close-date > today OR cust-role.close-date EQ ?)
                        AND CAN-DO("��४��,vlad1,vlad2", 
                                   cust-role.class-code)  NO-LOCK,
       EACH class OF cust-role NO-LOCK
       :
/*      mPosition[1] = GetXAttrValueEx("cust-role", 
                                     STRING(cust-role.cust-role-id), 
                                     "Post", 
                                     "").*/


      IF TRIM(cust-role.cust-name) NE "" THEN DO:
         mName = cust-role.cust-name.

         IF cust-role.cust-cat EQ "�" AND TRIM(cust-role.cust-id) NE ""  THEN
         DO:

            FIND FIRST person WHERE person.person-id EQ INT64(cust-role.cust-id)
               NO-LOCK NO-ERROR.
               
            IF AVAILABLE person THEN DO:
               teleph = "���." + trim(person.phone[1]) + "���.,���. " + trim(person.phone[2]).
	    END.
         END.

      END.	 	
      mClassNm = class.name.

      IF cust-role.close-date NE ? THEN 
         mStrTMP = STRING(cust-role.close-date, "99/99/9999").
      ELSE
         mStrTMP = "".

	   IND1 = IND1 + 1.	

      IF IND1 = 1 THEN PUT UNFORMATTED  trim(mClassNm) ";" trim(mName) ";" trim(teleph) SKIP.

     IF IND1 > 1 THEN PUT UNFORMATTED ";;;;;" trim(mClassNm) ";" trim(mName) ";" trim(teleph) SKIP.

   END.


END.




{preview.i}

