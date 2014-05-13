{globals.i}
{setdest.i}

DEF VAR klient AS CHAR NO-UNDO.
DEF VAR tm AS CHAR NO-UNDO.
DEF VAR tm1 AS CHAR NO-UNDO.
DEF VAR teleph AS CHAR NO-UNDO.

PUT UNFORMATTED "Заявки c 01/10/13 по " + string(end-date) SKIP(2).

FOR EACH loan 
	WHERE loan.contract EQ "Заявки" AND loan.open-date >= date("01/10/13")
        AND NOT CAN-DO("99", loan.cont-type) no-lock:
	klient = "".
        find first person where person.person-id = loan.cust-id no-lock.
        klient = person.name-last + " " + person.first-names.
        tm = GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"ДопОфис", ?).
        find first _user where _user._Userid = loan.user-id.
        tm1 = _user._User-Name.
        teleph = "Дом." + trim(person.phone[1]) + " Раб.,моб. " + trim(person.phone[2]).
	PUT UNFORMATTED /*loan.amt-rub ";" */loan.cont-code ";" loan.cont-cli ";" loan.open-date ";" loan.since ";" loan.currency ";" loan.cust-id ";" klient ";" loan.loan-status ";" tm ";" loan.user-id ";" tm1 ";" loan.branch ";" teleph ";" person.address SKIP.
end.


PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}





