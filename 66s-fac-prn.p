{globals.i}
{66base.fun}
{wordwrap.def}
{branch-types.def}

DEF INPUT PARAM RID AS RecID NO-UNDO.

DEF VAR vMerchantName    AS CHARACTER            NO-UNDO.
DEF VAR vMerchantAddr    AS CHARACTER            NO-UNDO.
DEF VAR vMerchantINN     AS CHARACTER            NO-UNDO.
DEF VAR vMerchantKPP     AS CHARACTER            NO-UNDO.
DEF VAR vSenderName      AS CHARACTER            NO-UNDO.
DEF VAR vSenderAddr      AS CHARACTER            NO-UNDO.
DEF VAR vNumSF           AS CHARACTER            NO-UNDO.
DEF VAR vPayeeDocNum     AS CHARACTER            NO-UNDO.
DEF VAR vPayeeDocDate    AS CHARACTER            NO-UNDO.
DEF VAR vCustomerName    AS CHARACTER            NO-UNDO.
DEF VAR vCustomerAddr    AS CHARACTER            NO-UNDO.
DEF VAR vCustomerINN     AS CHARACTER            NO-UNDO.
DEF VAR vCustomerKPP     AS CHARACTER            NO-UNDO.
/*Def Var CEO              As Character            No-Undo.*/
/*Def Var CFO              As Character            No-Undo.*/


DEF VAR vTName           AS CHARACTER EXTENT  10 NO-UNDO.
DEF VAR vUnit            AS CHARACTER            NO-UNDO.
DEF VAR vCount           AS INTEGER              NO-UNDO.
DEF VAR vCounter         AS INTEGER              NO-UNDO.
DEF VAR vPrice           AS DECIMAL              NO-UNDO.
DEF VAR vCost            AS DECIMAL              NO-UNDO.
DEF VAR vRateNDS         AS DECIMAL              NO-UNDO.
DEF VAR vSumNDS          AS DECIMAL              NO-UNDO.
DEF VAR vCostAll         AS DECIMAL              NO-UNDO.
DEF VAR vDirKO           AS CHARACTER            NO-UNDO.
DEF VAR vGBKO            AS CHARACTER            NO-UNDO.

DEF VAR vAccDt           AS CHARACTER            NO-UNDO.
DEF VAR vOp              AS INTEGER              NO-UNDO.
DEF VAR vNTrans          AS INTEGER              NO-UNDO.
DEF VAR vDocDate         AS DATE                 NO-UNDO.
DEF VAR vUserId          AS CHARACTER            NO-UNDO.
DEF VAR vOpKind          AS CHARACTER            NO-UNDO.

DEF VAR vTmpStr          AS Character            NO-UNDO.
DEF VAR vStrTemp         AS CHARACTER            NO-UNDO.
DEFINE BUFFER xbranch FOR branch.
/*DEFINE VAR ts AS CHAR NO-UNDO.  */


FUNCTION Date2StrR RETURNS CHAR
   (iDate AS DATE):

   DEF VAR vMonName    AS CHAR     NO-UNDO EXTENT 12 INITIAL
   [ "января" , "февраля", "марта"   ,
     "апреля" , "мая"    , "июня"    ,
     "июля"   , "августа", "сентября",
     "октября", "ноября" , "декабря"
   ].
   DEF VAR vStr   AS CHAR NO-UNDO.

   vStr = STRING(DAY(iDate), "99") + " " +
          vMonName[ MONTH(iDate) ] + " " +
          STRING(YEAR(iDate), "9999") + " г."
   NO-ERROR.
   RETURN vStr.
END FUNCTION.


/*НДС*/
FOR FIRST op WHERE RecID(op) = RID NO-LOCK,
FIRST op-entry OF op
NO-LOCK:
 vMerchantName = "ОАО ""Россельхозбанк""".
 vMerchantAddr = "Гагаринский пер., д.3, г.Москва, 119034".
 vMerchantINN = TRIM(FGetSetting("ИНН",?,"")).
 vMerchantKPP = TRIM(FGetSetting("БанкКПП",?,"")).
 vSenderName = TRIM(FGetSetting("Банк",?,"")).
 vSenderAddr = TRIM(FGetSetting("Адрес_юр",?,"")).
 FOR FIRST acct WHERE acct.acct EQ op-entry.acct-db NO-LOCK:
   IF acct.cust-cat = "Ю" THEN
   DO:
    FIND FIRST cust-corp WHERE
               cust-corp.cust-id = acct.cust-id
    NO-LOCK NO-ERROR.
    IF AVAILABLE cust-corp THEN
     DO:
      CASE cust-corp.cust-id:
       WHEN 227  THEN vCustomerName = "ОАО ""Фирма Энергозащита""".
       WHEN 2444 THEN vCustomerName = TRIM(cust-corp.name-short).
       OTHERWISE vCustomerName = TRIM(cust-corp.cust-stat) + " " + TRIM(cust-corp.name-corp).
      END CASE.
      vCustomerINN = TRIM(cust-corp.inn).
      vCustomerKPP = TRIM(GetXAttrValue("cust-corp", STRING(cust-corp.cust-id), "КПП")).
      vCustomerAddr = GetClientAddressKF2("Ю", cust-corp.cust-id, "АдрЮр").
      IF vCustomerAddr EQ "" THEN
       vCustomerAddr = GetClientAddressKF2("Ю", cust-corp.cust-id, "АдрФакт").
      IF vCustomerAddr EQ "" THEN
       vCustomerAddr = GetClientAddressKF2("Ю", cust-corp.cust-id, "АдрПочт").
      IF cust-corp.cust-id EQ 3924 THEN
      DO:
       vCustomerName = "Закрытое акционерное общество ""Сибсталь""".
       vCustomerAddr = "660093, Красноярский край, г.Красноярск, ул.Семафорная, 263".
       vCustomerINN = "2464228048".
       vCustomerKPP = "245043001".
      END.
     END.
   END.
   IF CAN-DO("47423........6*,47423........7*,47423........8*", acct.acct) AND (acct.cust-cat = "В") THEN
   DO:
     vCustomerName = "Физическое лицо".
     vCustomerINN = "-".
     vCustomerKPP = "-".
     vCustomerAddr = "-".
   END.
   IF (NOT acct.acct BEGINS "47423") AND (acct.cust-cat = "В") THEN
   DO:
    vCustomerName = "ОАО ""Россельхозбанк""".
    vCustomerINN = TRIM(FGetSetting("ИНН",?,"")).
    vCustomerKPP = TRIM(FGetSetting("БанкКПП",?,"")).
    vCustomerAddr = "пер.Гагаринский, д.3, г.Москва, 119034".
   END.
 END.
 vCount = INTEGER(GetXAttrValue("op", STRING(op.op), "Кол-во")).
 vRateNDS = DECIMAL(GetXAttrValue("op", STRING(op.op), "Ставка")).
 IF vRateNDS EQ 0 THEN
  vRateNDS = 18.
 vNumSF = TRIM(GetXAttrValue("op", STRING(op.op), "НомерСФ")).
 vSumNDS = op-entry.amt-rub.
 vOp = op.op.
 vPayeeDocDate = Date2StrR(op.op-date).
 vUserId = op.user-id.
 vOpKind = op.op-kind.
 vNTrans = op.op-transaction.
 vAccDt = op-entry.acct-db.
 vDocDate = op.op-date.
END.


/*  Вычисление значения специального поля CEO */
/*ts = "".
vTmpStr = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","6600").
IF {assigned vTmpStr} THEN DO:
   FIND FIRST xbranch WHERE xbranch.branch-id = vTmpStr NO-LOCK NO-ERROR.
   IF AVAIL xbranch
      THEN vTmpStr = xbranch.mgr-name.
      ELSE vTmpStr = "".
END.

CEO = ts + "" + (IF {assigned vTmpStr}
                               THEN vTmpStr
                               ELSE "")
.*/
/* Вычисление значения специального поля CFO */
/*ts = "Главный бухгалтер".
vTmpStr = GetXAttrValueEx("_user",USERID("bisquit"),"Отделение","6600").
IF {assigned vTmpStr} THEN DO:
   FIND FIRST xbranch WHERE xbranch.branch-id = vTmpStr NO-LOCK NO-ERROR.
   IF AVAIL xbranch THEN
      vTmpStr = xbranch.cfo-name.
   ELSE vTmpStr = "".
END.
 
CFO = ts + " _________________________ " + (IF {assigned vTmpStr}
                               THEN vTmpStr
                               ELSE "").*/



vPayeeDocNum = "-".
FOR FIRST op WHERE op.op <> vOp AND
                   op.op-date  EQ vDocDate AND
                   op.op-transaction EQ vNTrans AND
                   CAN-DO("01,06o",op.doc-type)
NO-LOCK,
FIRST op-entry OF op WHERE 
      (op.doc-type EQ "01" AND op-entry.acct-db EQ vAccDt) OR
      (op.doc-type EQ "06o" AND op-entry.acct-cr EQ vAccDt)
NO-LOCK:
 vPayeeDocNum = op.doc-num.
END.

/*Комиссия или плата за аренду*/
FOR FIRST op WHERE op.op <> vOp AND
                   op.op-date  EQ vDocDate AND
                   CAN-DO("01,06",op.doc-type) AND
                   ((op.op-transaction EQ vNTrans) OR
                    ((vOpKind EQ "i-rshb") AND
                     (op.op-kind EQ "i-rshb") AND                            
                     (op.user-id EQ vUserId) AND
                     ((INTEGER(op.doc-num) + 1) EQ INTEGER(vPayeeDocNum))))
NO-LOCK,
FIRST op-entry OF op WHERE 
      op-entry.acct-db EQ vAccDt
NO-LOCK:
 vTName[1] = op.details.
 vCost = op-entry.amt-rub.
 vCostAll = vCost + vSumNDS.
 vPrice = IF vCount NE 0 THEN vCost / vCount ELSE 0.
 vUnit = "шт.".
END.

vTName[1] = ENTRY(1, vTName[1], "~n").
vTName[1] = REPLACE(vTName[1],"НАЧИСЛЕНИЕ КОМИССИИ","КОМИССИЯ").
vTName[1] = REPLACE(vTName[1],"НАЧИСЛЕНА КОМИССИЯ","КОМИССИЯ").

{wordwrap.i &s=vTName &l=35 &n=10}

IF vAccDt BEGINS "60311" THEN
  PUT UNFORMATTED "Аренда государственного (муниципального) имущества" SKIP(1).
  PUT UNFORMATTED "                                                                                                                                                                " SKIP.
  PUT UNFORMATTED "                                                                                                 Приложение 3                                                   " SKIP.
  PUT UNFORMATTED "                                                                                                 к Положению о порядке исчисления и уплаты налога               " SKIP.
  PUT UNFORMATTED "                                                                                                 на добавленную стоимость №66-П                                 " SKIP.
  PUT UNFORMATTED "                                                                                                 (приказ ОАО Россельхозбанк от 07.02.2011 №38-ОД)               " SKIP.
  PUT UNFORMATTED "                                                                                                 (в редакции приказа ОАО Россельхозбанк от 20.04.2012 №203-ОД)  " SKIP(1).
  PUT UNFORMATTED FILL(" ",50) "СЧЕТ-ФАКТУРА № 6600/" vNumSF " от " vPayeeDocDate FILL(" ",40) "Приложение №1                     " SKIP.
  PUT UNFORMATTED FILL(" ",50) "ИСПРАВЛЕНИЕ №       от" FILL(" ",54) "к постановлению Правительства     " SKIP.
  PUT UNFORMATTED "                                                                                                                              Российской Федерации              " SKIP.                               
  PUT UNFORMATTED "                                                                                                                              от 26.12.2011 № 1137              " SKIP.
/*PUT UNFORMATTED FILL(" ",50) "СЧЕТ-ФАКТУРА № 6600/" vNumSF " от " vPayeeDocDate SKIP.*/
/*PUT UNFORMATTED FILL(" ",50) "ИСПРАВЛЕНИЕ №       от" SKIP(2).*/
PUT UNFORMATTED "Продавец " vMerchantName SKIP.
PUT UNFORMATTED "Адрес " vMerchantAddr SKIP.
PUT UNFORMATTED "ИНН/КПП продавца " vMerchantINN "/" vMerchantKPP SKIP.
PUT UNFORMATTED "Грузоотправитель и его адрес " /*vSenderName ", " vSenderAddr*/ "-" SKIP.
IF vCustomerAddr NE "-" THEN
 vStrTemp = vCustomerName + ", " + vCustomerAddr.
ELSE
 vStrTemp = "-".
PUT UNFORMATTED "Грузополучатель и его адрес " /*vStrTemp*/ "-" SKIP.
IF vPayeeDocNum NE "-" THEN
 PUT UNFORMATTED "К платежному документу № " vPayeeDocNum " от " vPayeeDocDate SKIP.
ELSE
 PUT UNFORMATTED "К платежному документу № -" SKIP.
PUT UNFORMATTED "Покупатель " vCustomerName SKIP.
PUT UNFORMATTED "Адрес " vCustomerAddr SKIP.
IF vCustomerINN NE "-" THEN
 vStrTemp = vCustomerINN + "/" + vCustomerKPP.
ELSE
 vStrTemp = "-".
PUT UNFORMATTED "ИНН/КПП покупателя " vStrTemp SKIP.
PUT UNFORMATTED "Валюта: наименование, код  Российский рубль, 643" SKIP(1).

PUT UNFORMATTED "┌───────────────────────────────────┬───────────┬───────┬────────────┬──────────────┬──────────┬───────┬──────────────┬─────────────────┬────────────────┬──────────┐" SKIP.
PUT UNFORMATTED "│                                   │  Единица  │Коли-  │Цена (тариф)│  Стоимость   │  В том   │Нало-  │    Сумма     │Стоимость товаров│     Страна     │  Номер   │" SKIP.
PUT UNFORMATTED "│        Наименование товара        │ измерения │чество │ за единицу │   товаров    │  числе   │говая  │    налога,   │ (работ, услуг), │  происхождения │таможенной│" SKIP.
PUT UNFORMATTED "│       (описание выполненных       ├─┬─────────┤(объем)│  измерения │(работ,услуг),│  сумма   │ставка │предъявляемая │ имущественных   │   товара       │декларации│" SKIP.
PUT UNFORMATTED "│       работ, оказанных услуг),    │к│условное │       │            │ имущественных│  акциза  │       │  покупателю  │прав с налогом - ├────────┬───────┤          │" SKIP.
PUT UNFORMATTED "│        имущественного права       │о│обозначе-│       │            │ прав без     │          │       │              │     всего       │цифровой│краткое│          │" SKIP.
PUT UNFORMATTED "│                                   │д│ние(наци-│       │            │налога - всего│          │       │              │                 │  код   │наиме- │          │" SKIP.
PUT UNFORMATTED "│                                   │ │ональное)│       │            │              │          │       │              │                 │        │нование│          │" SKIP.
PUT UNFORMATTED "├───────────────────────────────────┼─┼─────────┼───────┼────────────┼──────────────┼──────────┼───────┼──────────────┼─────────────────┼────────┼───────┼──────────┤" SKIP.
PUT UNFORMATTED "│                1                  │2│   2а    │   3   │     4      │      5       │     6    │   7   │      8       │        9        │   10   │  10а  │    11    │" SKIP.
PUT UNFORMATTED "├───────────────────────────────────┼─┼─────────┼───────┼────────────┼──────────────┼──────────┼───────┼──────────────┼─────────────────┼────────┼───────┼──────────┤" SKIP.

PUT UNFORMATTED "│" vTName[1] FORMAT "x(35)"        "│-│    -    │   -   │     -      │" 
                                                                      vCost FORMAT ">>>>>>>>>>9.99" "│Без акциза│"
                                                                                   vRateNDS FORMAT ">>>>>9%" "│"
                                                                                            vSumNDS FORMAT ">>>>>>>>>>9.99" "│"
                                                                                                                    vCostAll FORMAT ">>>>>>>>>>>>>9.99" "│   -    │   -   │    -     │" SKIP.

DO vCounter = 2 TO 10:
 IF vTName[vCounter] NE "" THEN
  PUT UNFORMATTED 
                "│" vTName[vCounter] FORMAT "x(35)" "│ │         │       │            │              │          │       │              │                 │        │       │          │" SKIP.
END.

PUT UNFORMATTED "├───────────────────────────────────┴─┴─────────┴───────┴────────────┼──────────────┼──────────┴───────┼──────────────┼─────────────────┼────────┴───────┴──────────┘" SKIP.
PUT UNFORMATTED "│Всего к оплате                                                      │              │         X        │" vSumNDS FORMAT ">>>>>>>>>>9.99" "|" vCostAll FORMAT ">>>>>>>>>>>>>9.99" "│" SKIP.

PUT UNFORMATTED "└────────────────────────────────────────────────────────────────────┴──────────────┴──────────────────┴──────────────┴─────────────────┘" SKIP(2).

put unformatted "Руководитель организации                                                                          Главный бухгалтер" skip.
put unformatted "или иное уполномоченное лицо  ______________________ Усольцева М.В.                               или иное уполномоченное лицо  ______________________ Кобычева К.О." skip(1). 

PUT UNFORMATTED "Уполномоченное приказом № 33 от 01.02.2013 г.                                                     уполномоченное приказом № 066/57 от 04.03.2014" SKIP(1).

PUT UNFORMATTED "(индивидуальный предприниматель)  ______________________________________    (реквизиты свидетельства о        " SKIP.
PUT UNFORMATTED "                                                                             государственной регистрации      " SKIP.
PUT UNFORMATTED "                                                                             индивидуального предпринимателя) " SKIP.

PUT UNFORMATTED "Примечание. Первый экземпляр - покупателю, второй экземпляр - продавцу." SKIP.
