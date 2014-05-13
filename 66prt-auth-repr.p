/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2011 ЗАО "Банковские информационные системы"
     Filename: prt-auth-repr.p
      Comment: Выписка по лицевому счету за период (Excel)
   Parameters:
         Uses:
      Used by:
      Created: 28.03.2011 kraa 0140872 
     Modified: 
*/

{globals.i}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{intrface.get xclass} 
{intrface.get prnvd}
{pp-uni.var &FILE_sword_p=YES}
{pp-uni.prg}
{tmprecid.def}
{sh-defs.i}
/*{bank-id.i}
{op-ident.i}
{w-pril.i} */

{intrface.get acct}
{intrface.get cust}                /* Библиотека для работы с клиентами. */


&SCOPED-DEFINE OP-SURROGATE-SYSCONF "signatures-op-surrogate"

DEFINE VARIABLE mName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vName1        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vName2        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vInn          AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAcct         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vClAcct       AS CHARACTER NO-UNDO.
DEFINE VARIABLE vCorAcct      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInnSend      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctSend     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNameSend     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInnRec       AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctRec      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mNameRec      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mText         AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE vType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vName         AS CHARACTER NO-UNDO.

DEFINE BUFFER xacct         FOR acct.
DEFINE BUFFER cracct        FOR acct.
DEFINE BUFFER dbacct        FOR acct.

DEFINE TEMP-TABLE ttPrtHead NO-UNDO
   FIELD clUser     AS CHARACTER
   FIELD acct     LIKE acct.acct
   FIELD acctName LIKE acct.details
   FIELD DPDDate    AS CHARACTER 
   FIELD acctBal    AS DECIMAL
.

DEFINE TEMP-TABLE ttPrtAcct NO-UNDO
   FIELD op       LIKE op.op 
   FIELD op-entry LIKE op-entry.op-entry 
   FIELD opDate     AS DATE 
   FIELD acct     LIKE acct.acct
   FIELD nDoc     LIKE op.doc-num
   FIELD DPDDate    AS CHARACTER 
   FIELD BIC      LIKE op-bank.bank-code
   FIELD clAcct     AS CHARACTER
   FIELD clName     AS CHARACTER
   FIELD clINN      AS CHARACTER
   FIELD clBankName AS CHARACTER
   FIELD contract   AS CHARACTER   
   FIELD amtRubCr   AS DECIMAL  
   FIELD amtRubDb   AS DECIMAL 
.

PROCEDURE GetInn:
   DEFINE INPUT  PARAMETER iName AS CHARACTER.
   DEFINE OUTPUT PARAMETER oName AS CHARACTER.
   DEFINE OUTPUT PARAMETER oInn  AS CHARACTER.

   DEFINE VARIABLE i AS INT64 NO-UNDO.
   
   DO:
      i = 4.
      
      DO WHILE LOOKUP(SUBSTRING(iName,i,1)," ,0,1,2,3,4,5,6,7,8,9") > 0 :
         i = i + 1.
      END.
      IF i EQ 4 THEN
         ASSIGN
            oInn  = ""
            oName = iName
         .
      ELSE
         ASSIGN
            oInn  = trim(substring(iName,5,i - 5))
            oName = substring(iName,i)
         . 
   END. /* IF iName BEGINS "ИНН " */
END PROCEDURE.

{getdates.i}



FOR EACH tmprecid,
  FIRST xacct WHERE RECID(xacct) = tmprecid.id NO-LOCK:
      IF NOT AVAIL(xacct) THEN NEXT.
      vAcct = DelFilFromAcct(xacct.acct).

	{empty ttPrtHead}
	{empty ttPrtAcct}

      
      /* Формирование поля клиент */
      IF xacct.cust-id NE ? THEN DO:
         CASE xacct.cust-cat:                                        
            WHEN "Ю" THEN
               RUN GetCustNameFormatted IN h_cust (xacct.cust-cat,
                                                   xacct.cust-id,
                                                   OUTPUT mName).
            OTHERWISE DO:
               RUN GetCustName IN h_base (
                     xacct.cust-cat,
                     xacct.cust-id,
                     xacct.acct,
                     OUTPUT vName1,
                     OUTPUT vName2,
                     INPUT-OUTPUT vInn
               ).
               mName = TRIM (vName1 + " " + vName2).                
            END.
         END CASE.
      END.
      
      RUN acct-pos IN h_base (xacct.acct,
                              xacct.currency,
                              beg-date,
                              beg-date,
                              CHR(251)).

      FIND LAST op-entry WHERE op-entry.op-date LE end-date
                           AND op-entry.op-date GE beg-date
                           AND (op-entry.acct-cr EQ xacct.acct
                            OR  op-entry.acct-db EQ xacct.acct)
                           AND op-entry.op-status GE gop-status
      NO-LOCK NO-ERROR.                                               
      CREATE ttPrtHead.
      ASSIGN
         ttPrtHead.clUser   = mName
         ttPrtHead.acct     = vAcct
         ttPrtHead.acctName = xacct.details
         ttPrtHead.acctBal  = (IF xacct.currency EQ "" THEN
                                  ABS(sh-in-bal)
                               ELSE
                                  ABS(sh-in-val))
         ttPrtHead.DPDDate  = (IF AVAIL (op-entry) THEN 
                                  STRING(op-entry.op-date)
                               ELSE
                                  "")                                   
      .
    
      FOR EACH op-entry WHERE (op-entry.acct-cr EQ vAcct
                                 OR op-entry.acct-db EQ vAcct)
                                AND op-entry.op-date LE end-date
                                AND op-entry.op-date GE beg-date
                                AND op-entry.op-status GE gop-status
      NO-LOCK:
         FIND FIRST op WHERE op.op EQ op-entry.op NO-LOCK NO-ERROR.
         IF NOT AVAIL(op) THEN NEXT.
         ASSIGN
            mInnSend      = ""
            mAcctSend     = ""
            mNameSend     = ""
            mInnRec       = ""
            mAcctRec      = ""
            mNameRec      = ""
         .
         {justasec} 
         {empty Info-Store}                                          
         vCorAcct = (IF op-entry.acct-cr EQ xacct.acct THEN
                        op-entry.acct-db
                     ELSE
                        op-entry.acct-cr).
         IF NOT CAN-DO("202*",xacct.acct) 
            AND NOT CAN-DO("202*",vCorAcct)
         THEN DO:         

            RUN Collection-Info.
   
            RUN for-pay("ДЕБЕТ,ПЛАТЕЛЬЩИК,БАНКПЛ,БАНКГО,БАНКФИЛ",
                        "ПП",
                        OUTPUT PlName[1],
                        OUTPUT PlLAcct,
                        OUTPUT PlRKC[1],
                        OUTPUT PlCAcct,
                        OUTPUT PlMFO).
            RUN for-rec("КРЕДИТ,ПОЛУЧАТЕЛЬ,БАНКПОЛ,БАНКГО,БАНКФИЛ",
                        "ПП",
                        OUTPUT PoName[1],
                        OUTPUT PoAcct,
                        OUTPUT PoRKC[1],
                        OUTPUT PoCAcct,
                        OUTPUT PoMFO).
      
            RUN GetInn(INPUT PlName[1], OUTPUT PlName[1], OUTPUT mInnSend).
            RUN GetInn(INPUT PoName[1], OUTPUT PoName[1], OUTPUT mInnRec).
            ASSIGN
               vInn = (IF op-entry.acct-cr EQ xacct.acct THEN
                             mInnSend
                          ELSE
                             mInnRec)  
               vName = (IF op-entry.acct-cr EQ xacct.acct THEN
                             PlName[1]
                          ELSE
                             PoName[1])
               vClAcct = (IF op-entry.acct-cr EQ xacct.acct THEN
                             PlLAcct
                          ELSE
                             PoAcct)  
            .

         END.
         /* если кассовый документ */         
         ELSE DO: 
            ASSIGN
               PlCAcct  = ""
               PoCAcct  = ""
               PlRKC[1] = ""
               PlMFO    = ""
               PoMFO    = ""
               PoRKC[1] = ""
            . 
 
            /* Определение типа кассового документа */
            {find-act.i 
                &bact = cracct
                &acct  = op-entry.acct-cr 
                &curr  = op-entry.currency
            }
            {find-act.i
               &bact = dbacct
               &acct  = op-entry.acct-db
               &curr  = op-entry.currency
            }
            IF NOT AVAIL(cracct) OR NOT AVAIL(dbacct) THEN NEXT.
            IF cracct.contract BEGINS "Касса" AND dbacct.contract BEGINS "Касса" THEN
                  vType = "приходно─расходный".
            ELSE DO:
               IF cracct.contract BEGINS "Касса" THEN
                  vType = "Расход".
               ELSE IF dbacct.contract BEGINS "Касса" THEN 
                  vType = "Приход".
            END.
            /* Расходный */ 
            IF vType EQ "Расход" THEN DO:
               vClAcct = op-entry.acct-cr.
                /* Получатель */
                RUN GetKasRec(INPUT op-entry.acct-cr, OUTPUT vName, OUTPUT vInn).
            END.

            /* Приходный */ 
            IF vType EQ "Приход" THEN DO:
               vClAcct = op-entry.acct-db.
               /* Плательщик */ 
               RUN GetKasSend(INPUT op-entry.acct-db, OUTPUT vName, OUTPUT vInn).
            END.

            /* Приходно-расходный */ 
            IF vType EQ "приходно─расходный" THEN DO:
               vClAcct = "".
               vName = "".
            END.   
         END.

         IF NOT CAN-FIND(FIRST ttPrtAcct WHERE ttPrtAcct.op-entry EQ op-entry.op-entry AND
                                               ttPrtAcct.op       EQ op.op)
         THEN DO:
            CREATE ttPrtAcct.
            ASSIGN
               ttPrtAcct.op       = op.op 
               ttPrtAcct.op-entry = op-entry.op-entry 
               ttPrtAcct.opDate = op.op-date
               ttPrtAcct.nDoc   = op.doc-num
               ttPrtAcct.acct   = (IF op-entry.acct-cr EQ xacct.acct THEN
                                      PlCAcct
                                   ELSE
                                      PoCAcct)                                        
               ttPrtAcct.clAcct = vClAcct
               ttPrtAcct.BIC    = (IF op-entry.acct-cr EQ xacct.acct THEN
                                      PlMFO
                                   ELSE
                                      PoMFO)
               ttPrtAcct.clName = vName
               ttPrtAcct.clINN  = vInn
               ttPrtAcct.clBankName = (IF op-entry.acct-cr EQ xacct.acct THEN
                                          PlRKC[1]
                                       ELSE
                                          PoRKC[1])
               ttPrtAcct.contract   = op.details
               ttPrtAcct.amtRubCr   = (IF op-entry.acct-cr EQ xacct.acct THEN
                                          op-entry.amt-rub
                                       ELSE
                                          0.0)                                                        
               ttPrtAcct.amtRubDb   = (IF op-entry.acct-cr EQ xacct.acct THEN
                                          0.0
                                       ELSE
                                          op-entry.amt-rub)                                            
            .     
         END. 
      END.      

RUN Insert_TTName("bDate",STRING(beg-date)).
RUN Insert_TTName("eDate",STRING(end-date)).
     
FIND FIRST ttPrtHead NO-LOCK NO-ERROR.
   RUN Insert_TTName("Date","c "+ STRING(beg-date) + " по " + STRING(end-date)).
   RUN Insert_TTName("Name",ttPrtHead.clUser).
   RUN Insert_TTName("acct",ttPrtHead.acct).  
   RUN Insert_TTName("acctName",ttPrtHead.acctName).   
   RUN Insert_TTName("DPDDate",ttPrtHead.DPDDate).    
   RUN Insert_TTName("acctBal",STRING(ttPrtHead.acctBal)).     

   FOR EACH ttPrtAcct 
   NO-LOCK BY ttPrtAcct.opDate BY ttPrtAcct.nDoc:              
      mText = mText
             +           (IF STRING(ttPrtAcct.opDate) NE ? THEN STRING(ttPrtAcct.opDate) ELSE "")      + "~n"       /*  1 */
             + "[@]~n" + (IF ttPrtAcct.nDoc NE ? THEN ttPrtAcct.nDoc ELSE "")                          + "~n[@/]~n" /*  2 */
             + "[@]~n" + (IF ttPrtAcct.BIC NE ? THEN ttPrtAcct.BIC ELSE "")                            + "~n[@/]~n" /*  3 */
             + "[@]~n" + (IF ttPrtAcct.acct NE ? THEN ttPrtAcct.acct ELSE "")                          + "~n[@/]~n" /*  4 */
             +           (IF ttPrtAcct.clBankName NE ? THEN ttPrtAcct.clBankName ELSE "")              + "~n"       /*  5 */
             + "[@]~n" + (IF ttPrtAcct.clAcct NE ? THEN ttPrtAcct.clAcct ELSE "")                      + "~n[@/]~n" /*  6 */
             +           (IF ttPrtAcct.clName NE ? THEN REPLACE(ttPrtAcct.clName,"~n","") ELSE "")     + "~n"       /*  7 */
             + "[@]~n" + (IF ttPrtAcct.clINN NE ? THEN ttPrtAcct.clINN ELSE "")                        + "~n[@/]~n" /*  8 */
             +           (IF STRING(ttPrtAcct.amtRubDb) NE ? THEN STRING(ttPrtAcct.amtRubDb,"-zzz,zzz,zzz,zz9.99") ELSE "")  + "~n"       /*  9 */
             +           (IF STRING(ttPrtAcct.amtRubCr) NE ? THEN STRING(ttPrtAcct.amtRubCr,"-zzz,zzz,zzz,zz9.99") ELSE "")  + "~n"       /* 10 */
             +           (IF ttPrtAcct.contract NE ? THEN REPLACE(ttPrtAcct.contract,"~n","") ELSE "") + "~n"       /* 11 */
      .             
   END. 

SUBSCRIBE TO "get-ttnames" ANYWHERE RUN-PROCEDURE "get-ttnames".  
RUN Insert_TTName("table1","") .
UNSUBSCRIBE TO "get-ttnames".    
RUN PrintSignatures.

RUN printvd.p ("prtAuthRep", INPUT TABLE ttNames).


PROCEDURE get-ttnames:
   DEFINE PARAMETER BUFFER ttnames FOR ttnames.
   IF ttnames.tvalue = "" THEN
   COPY-LOB mText TO ttnames.tlong.
END PROCEDURE.

/* Формирование подписей */
PROCEDURE PrintSignatures.
   DEFINE VARIABLE mSignaturIAttrCode   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mSignaturIAttrFormat AS CHARACTER NO-UNDO.

   DEFINE VARIABLE need-sign       AS CHARACTER  NO-UNDO INIT "".
   DEFINE VARIABLE vNumUserProcInt AS INT64    NO-UNDO .

   DEFINE VARIABLE vEntrySignaturI AS INT64    NO-UNDO.
   DEFINE VARIABLE vEntrySignaturJ AS INT64    NO-UNDO.
   DEFINE VARIABLE vSignaturValue  AS MEMPTR      NO-UNDO.

   DEFINE VARIABLE vRet1           AS CHARACTER  NO-UNDO INIT "".
   DEFINE VARIABLE vStrTMP         AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE vStrTMP1        AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mItem           AS INT64      NO-UNDO.   
   
   &GLOBAL-DEFINE DEF-SIGN "Дир,ГлБух,МестоПечати,Исполн,Дата"

   &SCOPED-DEFINE SignaturBranchId dept.branch

   /* Смотрим настройку подписей в процедуре */
  vNumUserProcInt = INT64(GetSysConf("user-proc-id")).

  FIND FIRST user-proc WHERE RECID(user-proc) EQ vNumUserProcInt
  NO-LOCK NO-ERROR.

  IF AVAILABLE user-proc THEN
     need-sign = GetXAttrValue("user-proc",string(user-proc.public-number),"Подписи").

     /* Если списка подписей нет, то устанавливаем стандартный список */
     IF need-sign EQ "" THEN
        need-sign = {&DEF-SIGN}.

     vRet1 = "".

     DO vEntrySignaturI = 1 TO NUM-ENTRIES (need-sign):
         /* Формирование подписи в область памяти */
        RUN signatur_rz.p (ENTRY(vEntrySignaturI,need-sign),
                           {&SignaturBranchId},
                           gend-date,
                           OUTPUT vSignaturValue).
        vStrTMP = TRIM(GET-STRING(vSignaturValue, 1)) NO-ERROR.

        DO vEntrySignaturJ = 1 TO NUM-ENTRIES(vStrTMP, "│"):
           vStrTMP1 = TRIM(ENTRY(vEntrySignaturJ, vStrTMP, "│")).

           IF vStrTMP1 NE "" THEN
              {additem2.i vRet1 vStrTMP1 "|"}
        END.
      END.      
   RUN DeleteOldDataProtocol IN h_base ({&OP-SURROGATE-SYSCONF}).
   DO mItem = 1 TO NUM-ENTRIES(vRet1, "|"):
      vStrTMP = ENTRY(mItem, vRet1, "|").
      RUN Insert_TTName("sign" + STRING(mItem), vStrTMP).
   END.   
END PROCEDURE.

PROCEDURE GetKasSend.
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oNameSend AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oSendInn AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE CustName      AS CHARACTER EXTENT 2 INIT "" NO-UNDO.
   DEFINE VARIABLE vInn          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mInn          AS CHARACTER NO-UNDO.
  
   CustName[1] = GetXAttrValueEx("op", STRING(op.op), "name-send", "").
   IF NOT {assigned CustName[1]} THEN DO:
      {find-act.i &acct=iAcct}
      CustName[1] = GetXAttrValueEx("op", STRING(op.op), "ФИО", "").
      IF CustName[1] = "" THEN
         CustName[1] = IF op.name-ben = ? THEN "" ELSE op.name-ben.
      IF CustName[1] = "" THEN DO:
        {getcust.i &name=CustName}
         run splitto2.p (CustName[1] + " " + CustName[2], 32, 44, output CustName[1], output CustName[2]).
         CustName[1] = CustName[1] + CustName[2].
         IF CAN-DO(FGetSetting("КассСчИНН","",?), SUBSTR(acct.acct,1,5))
            OR GetXAttrValueEx("acct", acct.acct + "," + acct.currency, "bank-inn", ?) = "Да"
         THEN DO:
            vINN        = IF FGetSetting("ИНН",?,?) <> "" THEN FGetSetting("ИНН",?,?) ELSE "000000000000".
            CustName[1] = (IF vINN <> "000000000000" THEN "ИНН " + vINN + " " ELSE "").
            CustName[1] = CustName[1] + FGetSetting("Банк",    ?,"").
         END.
      END.
   END.
   IF INDEX(CustName[1],"ИНН") = 1 THEN DO:
      RUN GetInn(INPUT CustName[1], OUTPUT CustName[1], OUTPUT mInn).
      IF vINN EQ "" THEN
         vInn = mInn.
   END.
   oSendInn  = vInn.
   oNameSend = CustName[1].
END PROCEDURE.

PROCEDURE GetKasRec.
   DEFINE INPUT  PARAMETER iAcct AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oNameRec AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oRecInn AS CHARACTER NO-UNDO.

   DEFINE VARIABLE CustName      AS CHARACTER EXTENT 2 INIT "" NO-UNDO.
   DEFINE VARIABLE vInn          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mInn          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mIdCustAttr   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vCustCat      AS CHARACTER NO-UNDO.

   CustName[1] = GetXAttrValueEx("op", STRING(op.op), "ФИО", "").
   IF CustName[1] = "" THEN
      CustName[1] = IF op.name-ben = ? THEN "" ELSE op.name-ben.
   IF CustName[1] = "" THEN DO:
      {find-act.i &acct=iAcct}
      IF acct.cust-cat EQ "В" THEN DO:
         RUN GetCustIdCli IN h_acct (INPUT  acct.acct + "," + acct.currency,
                                     OUTPUT vCustCat,
                                     OUTPUT mIdCustAttr).
         RUN GetCustName IN h_base (vCustCat, mIdCustAttr, ?,
                                    OUTPUT CustName[1],
                                    OUTPUT CustName[2],
                                    INPUT-OUTPUT vINN
         ).
         CustName[1] = CustName[1] + " " + CustName[2].
      END.
      IF TRIM(CustName[1]) EQ "" THEN DO:
         {getcust.i &name=CustName}
          run splitto2.p (CustName[1] + " " + CustName[2], 32, 44, output CustName[1], output CustName[2]).
          CustName[1] = CustName[1] + " " + CustName[2].
      END.
   END.

   IF INDEX(CustName[1],"ИНН") = 1 THEN DO:
      RUN GetInn(INPUT CustName[1], OUTPUT CustName[1], OUTPUT mInn).
      IF vINN EQ "" THEN
         vInn = mInn.
   END.
   oRecInn   = vINN.
   oNameRec  = CustName[1].          

END PROCEDURE.

message "Start"
view-as alert-box.


END.      


{intrface.del}          /* Выгрузка инструментария. */ 
