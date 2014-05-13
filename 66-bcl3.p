/*
               Банковская интегрированная система БИСквит
    Copyright: ОАО РОССЕЛЬХОЗБАНК
      Filename: 
      Comment: 
   Parameters
         Use:
      Used by:
      Created: 24/01/2013
     Modified: 29/01/2013 - добавлена проверка 455*
     Modified: 31/01/2013 - проверка на открытость 455*
*/

{globals.i}
{tmprecid.def}
{setdest.i}
{62-Lib.i}


DEF VAR klient       AS CHAR NO-UNDO.
DEF VAR teleph       AS CHAR NO-UNDO.
DEF VAR mProsrV      AS DECIMAL NO-UNDO.

put unformatted "Ведомость клиентов" skip. 
PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED "Код клиента;ФИО;телефоны;адрес;дата рождения;" SKIP.  


for each tmprecid no-lock, 
    first person where recid(person) = tmprecid.id no-lock:

	         assign klient = string(person.person-id) + ";" + person.name-last + " " + person.first-names.
	         teleph = "Дом." + trim(person.phone[1]) + " Раб.,моб. " + trim(person.phone[2]).
	         PUT UNFORMATTED STRING(klient,"x(40)") ";" teleph ";" person.address ";" STRING(person.birthday,"99/99/9999") ";" SKIP.    

END.

PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}