{globals.i}

def var cust-id_ like person.person-id label "Введите код клиента".
def var sel-id as char label "Введите тип клиента (Ч,Ю,П)".
def var data_cr  as date label "Введите дату создания анкеты (anketa)" init 03/30/11.
def var data_per as date label "Введите дату пересмотра анкеты (anketa)" init 03/30/14.

def buffer anketa   for loan.

DEF FRAME frame1 cust-id_ sel-id data_cr data_per
                 WITH CENTERED SIDE-LABELS TITLE "Только для анкет без истории".
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


IF sel-id="Ч" OR sel-id="Ю" OR sel-id="П" THEN DO:

find first anketa
     WHERE anketa.class-code    EQ 'АнкКлиента'
       AND anketa.contract      EQ 'АнкКлиента'
       AND anketa.cust-id = cust-id_
       AND anketa.cont-code BEGINS sel-id + ','.
anketa.open-date = data_cr.
anketa.since = data_per.


find first CODE 
	WHERE  code.class   EQ "АнкКлиента" 
        AND code.code BEGINS anketa.cont-code + CHR(1) + "Risk" + CHR(1).
code.misc[3] = string(data_cr).
code.misc[2] = string(data_per).


/* Вывод справочных сведений о анкете */
for each anketa 
     WHERE anketa.class-code    EQ 'АнкКлиента'
       AND anketa.contract      EQ 'АнкКлиента'
       AND anketa.cust-id = cust-id_
       AND anketa.cont-code BEGINS sel-id + ',':
  PUT " АНКЕТА " SKIP.
  PUT "" SKIP.
  put  anketa.cust-id ' Дата открытия ' anketa.open-date ' Дата пересмотра ' anketa.since skip.   
		
	PUT "" SKIP.
	PUT " УРОВЕНЬ РИСКА " SKIP.
	PUT "" SKIP.

	for each CODE
		WHERE code.class   EQ "АнкКлиента" 
			AND code.code BEGINS anketa.cont-code + CHR(1) + "Risk" + CHR(1).
	  put ' Начало риска ' code.misc[3]' Конец риска ' code.misc[2] skip.
	end.

end.

END.
ELSE   message "Несуществующий тип клиента" view-as alert-box.

/*for each anketa 
     WHERE anketa.class-code    EQ 'АнкКлиента'
       AND anketa.contract      EQ 'АнкКлиента'
       AND anketa.cust-id = 3320:
   put  anketa.cust-id ' ' anketa.cont-code ' Дата открытия ' anketa.open-date ' Дата пересмотра ' anketa.since skip.   
end.*/

{preview.i}.
