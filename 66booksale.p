/*
   Красноярский РФ ОАО "Россельхозбанк"
   Кириллов П.Г. 15.08.2007
   Выгрузка книги продаж
*/

{globals.i}

DEF INPUT PARAMETER iFileName AS CHAR.

DEF BUFFER b-op FOR op.
DEF BUFFER b-op-entry FOR op-entry.

DEF VAR vCustomerName AS CHAR NO-UNDO.
DEF VAR vCustomerINN  AS CHAR NO-UNDO.
DEF VAR vCustomerKPP  AS CHAR NO-UNDO.
DEF VAR vPriznak      AS CHAR NO-UNDO.

DEF VAR vCost    AS DECIMAL FORMAT "->>>>>>>>>9.99" NO-UNDO.
DEF VAR vCostAll AS DECIMAL FORMAT "->>>>>>>>>9.99" NO-UNDO.


{getdates.i}

OUTPUT TO VALUE(iFileName).

FOR EACH op WHERE 
         op.op-date >= beg-date AND
         op.op-date <= end-date AND
         CAN-DO("06",op.doc-type)
NO-LOCK,
FIRST op-entry OF op WHERE
          op-entry.acct-db BEGINS "47423" AND
          op-entry.acct-cr BEGINS "60309"
NO-LOCK
BREAK BY op.op-date
      BY INTEGER(GetXAttrValue("op", STRING(op.op), "НомерСФ")):

 /*наименование, ИНН, КПП клиента*/
 vCustomerName = "".
 vCustomerINN = "".
 vCustomerKPP = "".
 FOR FIRST acct WHERE acct.acct EQ op-entry.acct-db 
 NO-LOCK:
   IF acct.cust-cat = "Ю" THEN
   DO:
    FIND FIRST cust-corp WHERE
               cust-corp.cust-id = acct.cust-id
    NO-LOCK NO-ERROR.
    IF AVAILABLE cust-corp THEN
     DO:
      vCustomerName = TRIM(cust-corp.cust-stat) + " " + TRIM(cust-corp.name-corp).
      vCustomerINN = TRIM(cust-corp.inn).
      vCustomerKPP = TRIM(GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "КПП")).
     END.
   END.
   IF CAN-DO("47423........6*,47423........7*,47423........8*", acct.acct) AND (acct.cust-cat = "В") THEN
     vCustomerName = "Физическое лицо".
   IF (NOT acct.acct BEGINS "47423") AND (acct.cust-cat = "В") THEN
   DO:
    vCustomerName = "ОАО ""Россельхозбанк""".
    vCustomerINN = TRIM(FGetSetting("ИНН",?,"")).
    vCustomerKPP = TRIM(FGetSetting("БанкКПП",?,"")).
   END.
 END.

 /*Комиссия*/
 vCost = 0.
 vCostAll = 0.
 FOR FIRST b-op WHERE 
           b-op.op               <> op.op AND
           b-op.op-date          EQ op.op-date AND
           CAN-DO("06",b-op.doc-type) AND
           b-op.user-id          EQ op.user-id AND
           b-op.op-kind          EQ op.op-kind AND
           ((b-op.op-transaction EQ op.op-transaction) OR
           ((b-op.op-kind EQ "i-rshb") AND ((INTEGER(b-op.doc-num) + 1) EQ INTEGER(op.doc-num))))
 NO-LOCK,
 FIRST b-op-entry OF b-op WHERE
       b-op-entry.acct-db EQ op-entry.acct-db
 NO-LOCK:
  vCost = b-op-entry.amt-rub.
  vCostAll = vCost + op-entry.amt-rub.
 END.

 vPriznak = "".
 CASE op.op-kind:
   WHEN "com-22" THEN
     vPriznak = "П".
   WHEN "com-22_1" THEN
     vPriznak = "П".
   WHEN "com-22_2" THEN
     vPriznak = "П".
   WHEN "com-22_3" THEN
     vPriznak = "П".
   WHEN "com-X1" THEN
     vPriznak = "П".
   WHEN "com-X1do" THEN
     vPriznak = "П".
   WHEN "com-X2" THEN
     vPriznak = "П".
   WHEN "com-X3" THEN
     vPriznak = "П".
   WHEN "com-9091" THEN
     vPriznak = "П".
   WHEN "com-9092" THEN
     vPriznak = "П".
 END.

 PUT UNFORMATTED STRING(op.op-date, "99.99.9999") "#"
                 TRIM(GetXAttrValue("op", STRING(op.op), "НомерСФ")) "#"
                 vCustomerName "#"
                 vCustomerINN "#"
                 vCustomerKPP "#"
                 STRING(op.op-date, "99.99.9999") "#"
                 vCostAll "#"
                 vCost "#"
                 op-entry.amt-rub "#"
                 vPriznak SKIP.
END.


OUTPUT CLOSE.

MESSAGE "Данные успешно выгружены в файл" SKIP iFileName VIEW-AS ALERT-BOX.
