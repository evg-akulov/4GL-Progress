{globals.i}
{setdest.i}
{tmprecid.def}

def var klient as char.
def var teleph as char.


for each tmprecid, first loan where recid(loan) eq tmprecid.id NO-LOCK:
   find person where person.person-id = loan.cust-id no-lock no-error.

    if not can-find(first acct where 
		       acct.cust-cat = "Ч" 
                       AND acct.cust-id = person.person-id 
                       and acct.acct BEGINS "455"
                       and acct.contract = 'Кредит'
                       and acct.close-date = ?)
    then do: 

         assign klient = string(person.person-id) + ";" + person.name-last + " " + person.first-names.
               teleph = "Дом." + trim(person.phone[1]) + " Раб.,моб. " + trim(person.phone[2]).
         PUT UNFORMATTED loan.cont-code ";" loan.open-date ";" loan.end-date ";" STRING(klient,"x(41)") ";" teleph ";" person.address  SKIP.    

    end.
     
end. /* loan */          



PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}

