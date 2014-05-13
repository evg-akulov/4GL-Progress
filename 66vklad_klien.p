{globals.i}
{setdest.i}
{tmprecid.def}

def var klient as char.
def var teleph as char.


for each tmprecid, first loan where recid(loan) eq tmprecid.id NO-LOCK:

        find person where person.person-id = loan.cust-id no-lock no-error.
        if available person then
         assign klient = string(person.person-id) + ";" + person.name-last + " " + person.first-names.
               teleph = "Дом." + trim(person.phone[1]) + " Раб.,моб. " + trim(person.phone[2]).

 PUT UNFORMATTED loan.branch ";" loan.cont-code ";" loan.open-date ";" loan.end-date ";" STRING(klient,"x(40)") ";" teleph ";" person.address  SKIP.    
     
end. /* loan */          



PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}

