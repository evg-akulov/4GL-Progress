FUNCTION GetClientNameKF RETURNS CHAR (iCat AS CHAR, iID AS INTEGER):

  DEF VAR tmpResult AS CHAR NO-UNDO.

  tmpResult = "".

  IF iCat EQ "Ч" THEN DO:
   FIND FIRST person WHERE
              person.person-id EQ iID
   NO-LOCK NO-ERROR.
   IF AVAILABLE person THEN
    tmpResult = TRIM(person.name-last) + " " + person.first-names.
  END.

  IF iCat EQ "Ю" THEN DO:
   FIND FIRST cust-corp WHERE
              cust-corp.cust-id = iID
   NO-LOCK NO-ERROR.
   IF AVAILABLE cust-corp THEN
    tmpResult = TRIM(TRIM(cust-corp.cust-stat) + " " + cust-corp.name-corp).
  END.

  RETURN tmpResult.

END FUNCTION.


FUNCTION GetClientInnKF RETURNS CHAR (iCat AS CHAR, iID AS INTEGER):

  DEF VAR tmpResult AS CHAR NO-UNDO.

  tmpResult = "".

  IF iCat EQ "В" THEN 
   tmpResult = FGetSetting("ИНН","","").

  IF iCat EQ "Ч" THEN DO:
   FIND FIRST person WHERE
              person.person-id EQ iID
   NO-LOCK NO-ERROR.
   IF AVAILABLE person AND TRIM(person.inn) NE "0" THEN
    tmpResult = TRIM(person.inn).
  END.

  IF iCat EQ "Ю" THEN DO:
   FIND FIRST cust-corp WHERE
              cust-corp.cust-id = iID
   NO-LOCK NO-ERROR.
   IF AVAILABLE person AND TRIM(cust-corp.inn) NE "0" THEN
    tmpResult = TRIM(cust-corp.inn).
  END.

  RETURN tmpResult.

END FUNCTION.

FUNCTION GetClientAddressKF RETURNS CHAR (iCat AS CHAR, iID AS INTEGER, iTypeAddr AS CHAR):
/*АдрФакт и АдрПроп*/
  DEF VAR vAddress AS CHAR NO-UNDO.
  DEF VAR vResult AS CHAR NO-UNDO.
  DEF VAR vCounter AS INTEGER NO-UNDO.
  DEF VAR vCount AS INTEGER NO-UNDO.

  vAddress = "".
  vResult = "".
  FIND FIRST cust-ident WHERE
             cust-ident.cust-cat       EQ iCat         AND 
             cust-ident.cust-id        EQ iID          AND 
             cust-ident.close-date     EQ ?            AND 
             cust-ident.class-code     EQ "p-cust-adr" AND 
             cust-ident.cust-code-type EQ iTypeAddr
  NO-LOCK NO-ERROR.
  IF AVAILABLE cust-ident THEN 
   vAddress = cust-ident.issue.

  vCount = NUM-ENTRIES(vAddress).
  DO vCounter = 1 TO vCount:
   IF (TRIM(ENTRY(vCounter,vAddress)) NE "") AND (TRIM(ENTRY(vCounter,vAddress)) NE "000000") THEN
    IF vResult EQ "" THEN
     vResult = TRIM(ENTRY(vCounter,vAddress)).
    ELSE
     vResult = vResult + ", " + TRIM(ENTRY(vCounter,vAddress)).
  END.

  RETURN vResult.
END FUNCTION.

FUNCTION GetClientAddressKF2 RETURNS CHAR (iCat AS CHAR, iID AS INTEGER, iTypeAddr AS CHAR):
  DEF VAR vAddress AS CHAR NO-UNDO.
  DEF VAR vReg     AS CHAR NO-UNDO.
  DEF VAR vResult  AS CHAR NO-UNDO.
  DEF VAR vCounter AS INTEGER NO-UNDO.
  DEF VAR vCount   AS INTEGER NO-UNDO.

  vAddress = "".
  vResult = "".
  FIND FIRST cust-ident WHERE
             cust-ident.cust-cat       EQ iCat         AND 
             cust-ident.cust-id        EQ iID          AND 
             cust-ident.close-date     EQ ?            AND 
             cust-ident.class-code     EQ "p-cust-adr" AND 
             cust-ident.cust-code-type EQ iTypeAddr
  NO-LOCK NO-ERROR.
  IF AVAILABLE cust-ident THEN 
  DO:
   vAddress = cust-ident.issue.
   vReg     = GetXattrValue("cust-ident", cust-ident.cust-code-type + ',' + 
                                          cust-ident.cust-code      + ',' + 
                                          STRING(cust-ident.cust-type-num),
                            "КодРег").
  END.
  /*если регион - Москва, то не указываем его в адресе*/
  IF vReg EQ "00045" THEN
   vReg = "".
  FIND FIRST code WHERE
             code.class EQ "КодРег" AND
             code.code  EQ vReg
  NO-LOCK NO-ERROR.
  IF AVAILABLE code THEN
   vReg = code.name.
  IF TRIM(ENTRY(1,vAddress)) NE "" AND TRIM(ENTRY(1,vAddress)) NE "000000" THEN
   vResult = TRIM(ENTRY(1,vAddress)) + (IF vReg NE "" THEN ", " + vReg ELSE "").
  ELSE
   vResult = vReg.

  vCount = NUM-ENTRIES(vAddress).
  DO vCounter = 2 TO vCount:
   IF TRIM(ENTRY(vCounter,vAddress)) NE "" THEN
    vResult = vResult + ", " + TRIM(ENTRY(vCounter,vAddress)).
  END.

  RETURN vResult.
END FUNCTION.
