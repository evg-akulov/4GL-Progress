/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ЗАО "Банковские информационные системы"
     Filename: cvsacct.p
      Comment: Сведения о движении денежных средств по банковскому счету клиента
      Comment:
   Parameters: Строка ключевых параметров (Dir - каталог для отчетов)
         Uses: 
      Used by:
      Created: 19/01/2009 kraw (0022666)
     Modified: 13/05/2009 kraw (0107888) вместо 0 ненужное поле "сумма операции" пустое
     Modified: 16/07/2009 kraw (0113534) ИНН при помощи pd.i
*/
{globals.i}

&GLOBAL-DEFINE OFFSigns YES

{intrface.get tmess}

{tmprecid.def}
&GLOBAL-DEFINE multy-op-ontry YES
&GLOBAL-DEFINE allcur         YES
{pd.i}

DEFINE INPUT PARAMETER iParm AS CHARACTER NO-UNDO.

DEFINE VARIABLE mCustCat   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mCustId    AS INT64   NO-UNDO.
DEFINE VARIABLE mCatCorr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mStrTMP    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mItem1     AS INT64   NO-UNDO.
DEFINE VARIABLE mIsOpen    AS LOGICAL   NO-UNDO.

DEFINE VARIABLE mPlName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPlAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPlRKC     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPlCAcct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPlMFO     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPlInn     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPoName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPoAcct    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPoRKC     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPoCAcct   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPoMFO     AS CHARACTER NO-UNDO.
DEFINE VARIABLE mPoInn     AS CHARACTER NO-UNDO.

DEFINE VARIABLE mName      AS CHARACTER EXTENT 2 NO-UNDO.
DEFINE VARIABLE mINN       AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAmt1      AS CHARACTER          NO-UNDO.
DEFINE VARIABLE mAmt2      AS CHARACTER          NO-UNDO.

DEFINE VARIABLE mDocNum    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mOpDate    AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDtls      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mDirName   AS CHARACTER NO-UNDO.

DEFINE VARIABLE mString    AS CHARACTER NO-UNDO.

DEFINE BUFFER xacct FOR acct.
DEFINE BUFFER op-entry FOR op-entry.

DEFINE STREAM sCVS.

{parsin.def}

mDirName = GetParamByNameAsChar(iParm, "dir", "").

IF LENGTH(mDirName) GT 0 THEN
DO:

   IF OPSYS EQ "UNIX" THEN
   DO:

      IF SUBSTRING(mDirName, LENGTH(mDirName)) NE "/" THEN
         mDirName = mDirName + "/".
   END.
   ELSE
   DO:

      IF SUBSTRING(mDirName, LENGTH(mDirName)) NE "~\" THEN
         mDirName = mDirName + "~\".
   END.
END.

{getdates.i
   &TitleLabel="ПАРАМЕТРЫ ОТЧЕТА"
   &BegLabel  ="Начало периода"
   &EndLabel  =" Конец периода"
}

IF KEYFUNC(LASTKEY) EQ "end-error" THEN
   RETURN.

FUNCTION __cvs_string RETURNS CHARACTER (INPUT iStr AS CHARACTER):

   DEFINE VARIABLE vStrTMP  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vStrTMP1 AS CHARACTER NO-UNDO.

   vStrTMP = REPLACE(iStr, ";", ",").

   DO WHILE INDEX(vStrTMP,CHR(10)) GT 0:

      vStrTMP1 = SUBSTRING(vStrTMP, 1, INDEX(vStrTMP,CHR(10)) - 1)
               + (IF SUBSTRING(vStrTMP, INDEX(vStrTMP,CHR(10))) EQ " " THEN ""
                                                                           ELSE " ")
               + SUBSTRING(vStrTMP, INDEX(vStrTMP,CHR(10)) + 1).
      vStrTMP = vStrTMP1.
   END.
   RETURN vStrTMP.
END FUNCTION.

FOR EACH tmprecid,
   EACH acct WHERE RECID(acct) EQ tmprecid.id 
               AND acct.open-date LE end-date NO-LOCK:
   ASSIGN
      mCustCat = acct.cust-cat
      mCustId  = acct.cust-id
      mIsOpen  = NO
   .
   RUN GetCustName IN h_Base (mCustCat,
                              mCustId,
                              IF AVAILABLE acct THEN acct.acct ELSE "",
                              OUTPUT mName[1],
                              OUTPUT mName[2],
                              INPUT-OUTPUT mInn).
   IF mCustCat EQ "Ю" THEN DO:
      FIND FIRST code WHERE 
                 code.class EQ "КодПредп"
             AND code.val   EQ mName[1]   NO-LOCK NO-ERROR.
      IF AVAIL code THEN
         mName[1]  = code.name.
   END.

   FOR EACH op-entry WHERE     (    op-entry.op-date GE beg-date
                                AND op-entry.op-date LE end-date
                                AND op-entry.acct-db EQ acct.acct
                               )
                            OR (    op-entry.op-date GE beg-date
                                AND op-entry.op-date LE end-date
                                AND op-entry.acct-cr EQ acct.acct
                               ) 
      NO-LOCK,
       EACH op OF op-entry WHERE op.op-status GE gop-status NO-LOCK BY op.op-date:

/*  иркутск  

    обработка многопровод документов      */

      IF op-entry.acct-db EQ acct.acct THEN
      DO:
         mAmt1 = STRING(op-entry.amt-rub).
         mAmt2 = "".
      END.

      IF op-entry.acct-cr EQ acct.acct THEN
      DO:
         mAmt1 = "".
         mAmt2 = STRING(op-entry.amt-rub).
      END.


/*      message "d" mAmt1 "k" mAmt2
      view-as alert-box.	*/


      IF NOT mIsOpen THEN
         OUTPUT STREAM sCVS TO VALUE(mDirName + acct.number + ".cvs").

      ASSIGN
         mPlName  = ""
         mPlAcct  = ""
         mPlRKC   = ""
         mPlCAcct = ""
         mPlMFO   = ""
         mPoName  = ""
         mPoAcct  = ""
         mPoRKC   = ""
         mPoCAcct = ""
         mPoMFO   = ""
         mIsOpen = YES
      .

      {empty Info-Store}
      RUN Collection-Info.
      mCatCorr = "".

      FIND FIRST xacct WHERE xacct.acct     EQ op-entry.acct-db
                         AND xacct.currency EQ op-entry.currency
         NO-LOCK NO-ERROR.
         
      IF AVAILABLE xacct THEN
         mCatCorr = xacct.cust-cat.

      RUN for-pay("ДЕБЕТ,ПЛАТЕЛЬЩИК,БАНКПЛ,БАНКГО,БАНКФИЛ",
                  "ПП",
                  OUTPUT mPlName,
                  OUTPUT mPlAcct,
                  OUTPUT mPlRKC,
                  OUTPUT mPlCAcct,
                  OUTPUT mPlMFO).

      mPlInn = GetPayerINN(RECID(op)).

      FIND FIRST xacct WHERE xacct.acct     EQ op-entry.acct-cr
                         AND xacct.currency EQ op-entry.currency
         NO-LOCK NO-ERROR.
         
      IF AVAILABLE xacct THEN
         mCatCorr = xacct.cust-cat.

      RUN for-pay("КРЕДИТ,ПОЛУЧАТЕЛЬ,БАНКПОЛ,БАНКГО,БАНКФИЛ",
                  "ПП",
                  OUTPUT mPoName,
                  OUTPUT mPoAcct,
                  OUTPUT mPoRKC,
                  OUTPUT mPoCAcct,
                  OUTPUT mPoMFO).

      mPoInn = GetRecipientINN(RECID(op)).

      IF AVAILABLE xacct THEN
      DO:
         mStrTMP = FGetSetting("НазнСчКас", "", "Касса").

         IF CAN-DO(mStrTMP, xacct.contract) THEN
      END.

      IF mPlName BEGINS "ИНН" THEN
      DO:

         mItem1 = 4.

         DO WHILE LOOKUP(SUBSTRING(mPlName, mItem1, 1), " ,0,1,2,3,4,5,6,7,8,9") > 0 :
            mItem1 = mItem1 + 1.
         END.
         mPlName = SUBSTRING(mPlName, mItem1).
      END.

      IF mPoName BEGINS "ИНН" THEN
      DO:

         mItem1 = 4.

         DO WHILE LOOKUP(SUBSTRING(mPoName, mItem1, 1), " ,0,1,2,3,4,5,6,7,8,9") > 0 :
            mItem1 = mItem1 + 1.
         END.
         mPoName = SUBSTRING(mPoName, mItem1).
      END.

      IF op.op-date NE ? THEN
         mOpDate = STRING(op.op-date, "99/99/9999").
      ELSE
         mOpDate = "".

      IF {assigned op.doc-num} THEN
         mDocNum = op.doc-num.
      ELSE
         mDocNum = "".

      IF {assigned op.details} THEN
         mDtls = __cvs_string(op.details).
      ELSE
         mDtls = "".

      IF {assigned mPlRKC} THEN
         mPlRKC = __cvs_string(mPlRKC).
      ELSE
         mPlRKC = "".

      IF {assigned mPoRKC} THEN
         mPoRKC = __cvs_string(mPoRKC).
      ELSE
         mPoRKC = "".

      IF {assigned mPoName} THEN
         mPoName = __cvs_string(mPoName).
      ELSE
         mPoName = "".

      IF {assigned mPlName} THEN
         mPlName = __cvs_string(mPlName).
      ELSE
         mPlName = "".

      IF {assigned mPlInn} THEN
         mPlInn = mPlInn.
      ELSE
         mPlInn = "".

      IF {assigned mPoInn} THEN
         mPoInn = mPoInn.
      ELSE
         mPoInn = "".

/*         IF op-entry.currency EQ "" THEN
            message acct.acct op-entry.acct-db op-entry.acct-cr STRING(op-entry.amt-rub)
	    view-as alert-box.
         ELSE
            message acct.acct op-entry.acct-db op-entry.acct-cr STRING(op-entry.amt-cur)
	    view-as alert-box.*/



      mString = 
/*                                             1. номер платежного документа;                */
                                          mDocNum
/*                                             2. дату совершения операции;                  */
                                  + ";" + mOpDate
/*                                             3. наименование банка плательщика;            */
                                  + ";" + mPlRKC
/*                                             4. БИК (или SWIFT-код) банка плательщика;     */
                                  + ";" + mPlMFO
/*                                             5. наименование плательщика;                  */
                                  + ";" + mPlName
/*                                             6. ИНН плательщика (для резидентов);          */
                                  + ";" + mPlInn
/*                                             7. номер счета плательщика;                   */
                                  + ";" + mPlAcct
/*                                             8. наименование банка получателя;             */
                                  + ";" + mPoRKC
/*                                             9. БИК (или SWIFT-код) банка получателя;      */
                                  + ";" + mPoMFO
/*                                             10. наименование получателя;                  */
                                  + ";" + mPoName
/*                                             11. ИНН получателя (для резидентов);          */
                                  + ";" + mPoInn
/*                                             12. номер счета получателя;                   */
                                  + ";" + mPoAcct
/*                                             13. сумма операции по дебету счета;           */
                                  + ";" + mAmt1
/*                                             14. сумма операции по кредиту счета;          */
                                  + ";" + mAmt2
/*                                             15. назначение платежа (полное содержание).   */
                                  + ";" + mDtls
      .                                  
      RUN to1251.p(INPUT-OUTPUT mString).

      PUT STREAM sCVS UNFORMATTED mString
                                  SKIP.
   END.
   IF mIsOpen THEN
      OUTPUT STREAM sCVS CLOSE.
END.

MESSAGE "Процедура завершена." SKIP(1)
"Каталог вывода=~"" + mDirName + "~""
VIEW-AS ALERT-BOX. 

{intrface.del}


