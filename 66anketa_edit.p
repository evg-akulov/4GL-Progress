{globals.i}

def var cust-id_ like person.person-id label "������ ��� ������".
def var sel-id as char label "������ ⨯ ������ (�,�,�)".
def var data_cr  as date label "������ ���� ᮧ����� ������ (anketa)" init 03/30/11.
def var data_per as date label "������ ���� ���ᬮ�� ������ (anketa)" init 03/30/14.

def buffer anketa   for loan.

DEF FRAME frame1 cust-id_ sel-id data_cr data_per
                 WITH CENTERED SIDE-LABELS TITLE "���쪮 ��� ����� ��� ���ਨ".
pause 0.

display cust-id_ with frame frame1.
display sel-id with frame frame1.
display data_cr with frame frame1.
display data_per with frame frame1.


set cust-id_ with frame frame1.
set sel-id   with frame frame1.
set data_cr  with frame frame1.
set data_per with frame frame1.



clear.

{setdest.i &cols=96}


IF sel-id="�" OR sel-id="�" OR sel-id="�" THEN DO:

find first anketa
     WHERE anketa.class-code    EQ '���������'
       AND anketa.contract      EQ '���������'
       AND anketa.cust-id = cust-id_
       AND anketa.cont-code BEGINS sel-id + ','.
anketa.open-date = data_cr.
anketa.since = data_per.


find first CODE 
	WHERE  code.class   EQ "���������" 
        AND code.code BEGINS anketa.cont-code + CHR(1) + "Risk" + CHR(1).
code.misc[3] = string(data_cr).
code.misc[2] = string(data_per).


/* �뢮� �ࠢ���� ᢥ����� � ����� */
for each anketa 
     WHERE anketa.class-code    EQ '���������'
       AND anketa.contract      EQ '���������'
       AND anketa.cust-id = cust-id_
       AND anketa.cont-code BEGINS sel-id + ',':
  PUT " ������ " SKIP.
  PUT "" SKIP.
  put  anketa.cust-id ' ��� ������ ' anketa.open-date ' ��� ���ᬮ�� ' anketa.since skip.   
		
	PUT "" SKIP.
	PUT " ������� ����� " SKIP.
	PUT "" SKIP.

	for each CODE
		WHERE code.class   EQ "���������" 
			AND code.code BEGINS anketa.cont-code + CHR(1) + "Risk" + CHR(1).
	  put ' ��砫� �᪠ ' code.misc[3]' ����� �᪠ ' code.misc[2] skip.
	end.

end.

END.
ELSE   message "���������騩 ⨯ ������" view-as alert-box.

/*for each anketa 
     WHERE anketa.class-code    EQ '���������'
       AND anketa.contract      EQ '���������'
       AND anketa.cust-id = 3320:
   put  anketa.cust-id ' ' anketa.cont-code ' ��� ������ ' anketa.open-date ' ��� ���ᬮ�� ' anketa.since skip.   
end.*/

{preview.i}.
