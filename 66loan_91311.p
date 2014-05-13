/*
   Иркутский РФ ОАО "Россельхозбанк"
   Кириллов П.Г. 
*/

{globals.i}
{49base.fun}
{49loan.fun}
{tmprecid.def}
{setdest.i}


DEF VAR vDate       AS DATE LABEL "Дата отчета        " NO-UNDO.
DEF VAR vSum1       AS DECIMAL NO-UNDO.
DEF VAR vSum2       AS DECIMAL NO-UNDO.
DEF VAR vSum3       AS DECIMAL NO-UNDO.
DEF VAR mAcct1      AS CHAR NO-UNDO.
DEF VAR mAcct2      AS CHAR NO-UNDO.
DEF VAR mAcct3      AS CHAR NO-UNDO.

{getdate.i}
/*vDate = gend-date.*/
vDate = end-date.

put unformatted "Ведомость договоров с 91311 " skip(2).

for each tmprecid,
    each loan where recid(loan) = tmprecid.id
                and loan.close-date eq ?
                no-lock:
  
  mAcct1 = GetAcctByRole(loan.contract, loan.cont-code, "КредЦБум", vDate).
  mAcct2 = GetAcctByRole(loan.contract, loan.cont-code, "Кредит", vDate).
  mAcct3 = GetAcctByRole(loan.contract, loan.cont-code, "КредПр", vDate).

  vSum1 = GetRestByRole(loan.contract, loan.cont-code, "КредЦБум", vDate).
  vSum2 = GetRestByRole(loan.contract, loan.cont-code, "Кредит", vDate).
  vSum3 = GetRestByRole(loan.contract, loan.cont-code, "КредПр", vDate).
 
  PUT UNFORMATTED GetClientNameKF(loan.cust-cat, loan.cust-id) ";" mAcct1 ";" vSum1 ";" mAcct2 ";" vSum2 ";" mAcct3 ";" vSum3 ";"SKIP.

END. 

PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}


