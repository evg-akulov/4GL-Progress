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
{setdest.i}
{tmprecid.def}
{66base.fun}

DEF VAR klient         AS CHARACTER            NO-UNDO.
DEF VAR teleph         AS CHARACTER            NO-UNDO.
DEF VAR vCustomerAddr  AS CHARACTER            NO-UNDO.
DEF VAR ccBranch-id    AS CHARACTER            NO-UNDO.
DEF VAR vTmpStr        AS CHARACTER            NO-UNDO.
DEF VAR vDols          AS CHARACTER            NO-UNDO.
DEF VAR vFIO           AS CHARACTER            NO-UNDO.
DEF VAR vNDog          AS CHARACTER            NO-UNDO.
DEF VAR vIsp           AS CHARACTER            NO-UNDO.

for each tmprecid no-lock, 
    first cust-corp where recid(cust-corp) = tmprecid.id no-lock:

/*80 строк*/
PUT UNFORMATTED "                                                                     Приложение 13" SKIP.
PUT UNFORMATTED "                                                                     к Правилам открытия и закрытия в ОАО <Россельхозбанк>" SKIP.
PUT UNFORMATTED "                                                                     банковских счетов в валюте Российской Федерации" SKIP.
PUT UNFORMATTED "                                                                     и иностранной валюте юридическим лицам, индивидуальным " SKIP.
PUT UNFORMATTED "                                                                     предпринимателям и физическим лицам, занимающимся в" SKIP. 
PUT UNFORMATTED "                                                                     установленном законодательством Российской Федерации" SKIP.
PUT UNFORMATTED "                                                                     порядке частной практикой, № 105-П" SKIP.
PUT UNFORMATTED "                                                                     (приказ ОАО <Россельхозбанк> от 10.05.2007 № 136-ОД)" SKIP.
PUT UNFORMATTED "                                                                     (в редакции приказов ОАО <Россельхозбанк> от 29.12.2011" SKIP. 
PUT UNFORMATTED "                                                                      № 606-ОД, от 31.05.2013 № 268-ОД)" SKIP(4).

klient        = "".
vCustomerAddr = "".
ccBranch-id   = "".
vTmpStr       = "".
vDols         = "".
vFIO          = "".
vNDog         = "".
vIsp          = "".

       klient = cust-corp.cust-stat + " " + cust-corp.name-corp.
       teleph = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"tel", ?).
       vCustomerAddr = GetClientAddressKF2("Ю", cust-corp.cust-id, "АдрЮр").
       ccBranch-id = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"branch-id", ?).
   FIND FIRST branch WHERE branch.branch-id = ccBranch-id NO-LOCK NO-ERROR.
   IF AVAILABLE branch THEN DO:
    vTmpStr = branch.name.
    vDols = branch.mgr-title.
    vFIO = branch.mgr-name.
   END.

   IF ccBranch-id = "6600" THEN DO:
        vDols = "Начальник операционного отдела".
	vFIO  = "Яковлева А.А.".
   END.
     
   find first acct where    acct.cust-cat = "Ю" 
                       AND acct.cust-id = cust-corp.cust-id 
                       and (acct.acct BEGINS "40702" OR acct.acct BEGINS "40802")
                       and acct.contract = "Расчет"
                       and acct.close-date = ? NO-LOCK NO-ERROR.

   IF AVAILABLE acct THEN vNDog = GetXattrValueEx("acct",STRING(acct.acct) + "," + STRING(acct.currency),"ДогОткрЛС", ?).  

   FIND FIRST _user WHERE _user._userid EQ userid("bisquit") NO-LOCK NO-ERROR.
   IF AVAILABLE _user THEN vIsp = GetXattrValueEx("_user",_user._userid,"_user-name", "_________").


PUT UNFORMATTED "      " vTmpStr SKIP(4).
PUT UNFORMATTED "┌──────────────────────────────────────────────────────┬────────────────────────────────────────────────────────────────┐" SKIP.
PUT UNFORMATTED "│                                                      │" klient FORMAT "x(64)"                                        "│" SKIP.
PUT UNFORMATTED "│ <____>_________20___г. № __________________________  │                                                                │" SKIP.
PUT UNFORMATTED "│                                                      │" vCustomerAddr FORMAT "x(64)"                                 "│" SKIP.
PUT UNFORMATTED "└──────────────────────────────────────────────────────┴────────────────────────────────────────────────────────────────┘" SKIP(2).

PUT UNFORMATTED "О представлении информации и документов," SKIP.
PUT UNFORMATTED "необходимых для обновления сведений о клиенте" SKIP(3).

PUT UNFORMATTED "В соответствии с условиями Договора банковского счета, " + vNDog + " ранее заключенного с Вашей" SKIP.
PUT UNFORMATTED "организацией, Банк имеет право ежегодно направлять клиенту письменные запросы о представлении информации и документов, необходимых для обновления" SKIP. 
PUT UNFORMATTED "сведений о клиенте, имеющихся в Банке, а клиент обязан предоставлять такую информацию и документы." SKIP(1).

PUT UNFORMATTED "В связи с изложенным просим Вас предоставить в Банк необходимые документы и информацию о внесении изменений и дополнений в учредительные" SKIP.
PUT UNFORMATTED "документы, изменении адреса, телефона, реорганизации или ликвидации Вашей организации, о других изменениях, способных повлиять на исполнение Договора." SKIP(1).

PUT UNFORMATTED "Представленная Вами информация будет использована Банком для обновления сведений о Вашей организации, содержащихся в информационной системе" SKIP.
PUT UNFORMATTED "Банка, а документы будут помещены в юридическое дело Вашей организации, ведущееся в Банке." SKIP(4).

PUT UNFORMATTED "   " vDols  format "x(45)"                 " _________________       " vFIO   SKIP.
PUT UNFORMATTED "(должность уполномоченного лица Банка/               (подпись)           (расшифровка подписи)" SKIP.
PUT UNFORMATTED "регионального филиала/ дополнительного офиса)" SKIP(1).

PUT UNFORMATTED "Исп. " + vIsp    SKIP.
PUT UNFORMATTED "Тел. _________" SKIP(50).

end.

{preview.i}