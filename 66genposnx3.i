/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2000 ТОО "Банковские информационные системы"
     Filename: GENPOSN.I
      Comment: Новый инклюд для создания ведомости остатков лицевых счетов
   Parameters:
         Uses:INDEXED-REPOSITION
      Used by:
      Created: 20/03/00 Serge
     Modified: 20/03/00 Serge
     Modified: 12/10/00 Olenka - добавление колонки печати доп.реквизита
     Modified: 12/07/01 SAP - возможность одновременного показа в ведомости трех реквизитов
     Modified: 25/04/02 Gunk - много доп.реквизитов, возможность сохранения настроек, перемещение колонок...
     Modified: 02/09/02 Gunk - подитоги по валютам
     Modified: 09.12.2003 kraw (0022693) - вывод допреквизитов DECIMAL и INT64
     Modified: 05.05.2004 abko (0021091) - возможность сортировки по 5 полям.
     Modified: 24.06.2005 kraw (0044551) - Шапка и подвал из ДР
     Modified: 07.12.2005 kraw (0055107) - zerospace наоборот
     Modified: 17/05/2007 kraw (0076959) - 6 знакомест под номер страницы
     Modified: 16.06.2008 muta  0085907  - добавлена колонка "Счет резерва"
     Modified: 11/07/2011 kraw (0151367) - темпорированные ДР
*/

{globals.i}
{chkacces.i}
{sh-defs.i}
{sh-temp.i new}
{wordwrap.def}
{wclass.i}
{intrface.get acct}
{intrface.get xclass}
{xlink.def}

&GLOBAL-DEFINE no-list-class-acct_flt YES

DEFINE TEMP-TABLE wrkqty LIKE sh.

DEFINE TEMP-TABLE wrkacct NO-UNDO
   FIELD bal-acct  LIKE acct.bal-acct
   FIELD acct      LIKE acct.acct
   FIELD id        AS   RECID
   FIELD currency  LIKE acct.acct
   FIELD breaksrt  AS   CHARACTER
   FIELD srtfield  AS   CHARACTER
   FIELD since     AS   DATE
   FIELD val       AS   DECIMAL DECIMALS 2 EXTENT 12
   FIELD acctname  AS   CHARACTER
   FIELD cliname   AS   CHARACTER EXTENT 10
   FIELD cust-id   LIKE acct.cust-id	
   FIELD cont-code AS   CHARACTER
   FIELD acct-rsrv LIKE acct.acct
   INDEX bal-acct bal-acct breaksrt srtfield
.

/* Для перемещения колонок */
DEFINE TEMP-TABLE flds
   FIELD num   AS INT64
   FIELD sel   AS LOG
   FIELD sl    AS CHARACTER FORMAT "x(1)"
   FIELD nam   AS CHARACTER FORMAT "x(16)"
   FIELD id    AS CHARACTER INITIAL ""
   INDEX i1 num
.

/* Допреквизиты */
DEFINE TEMP-TABLE xattr-temp
   FIELD xattr-code  LIKE xattr.xattr-code
   FIELD length      AS   INT64
   FIELD data-format LIKE xattr.data-format
   FIELD DATA-TYPE   LIKE xattr.data-type
.

DEFINE TEMP-TABLE ttSort
   FIELD Sort1    AS CHARACTER 
   FIELD Sort2    AS CHARACTER
   FIELD Sort3    AS CHARACTER
   FIELD Sort4    AS CHARACTER
   FIELD Sort5    AS CHARACTER
   FIELD SortName AS CHARACTER
   FIELD RWD   AS ROWID
   INDEX idxSort Sort1 Sort2 Sort3 Sort4 Sort5
.

/* Для сортировки */
DEFINE VARIABLE vSortFlds     AS CHARACTER INITIAL "Балансовый счет,Валюта"      NO-UNDO.
DEFINE VARIABLE long-acct     AS CHAR FORMAT "x(25)" COLUMN-LABEL "ЛИЦЕВОЙ СЧЕТ" NO-UNDO.

DEFINE VARIABLE num-class     AS INT64                  NO-UNDO.
DEFINE VARIABLE list-class    AS CHARACTER                NO-UNDO.
DEFINE VARIABLE vCurrentCount AS INT64                  NO-UNDO.
DEFINE VARIABLE vCurrentTotal AS DECIMAL EXTENT 6         NO-UNDO.
DEFINE VARIABLE vPosCurrency  AS INT64 INITIAL 2        NO-UNDO.
DEFINE VARIABLE vPosBreaksrt  AS INT64 INITIAL 1        NO-UNDO.
DEFINE VARIABLE vPosCount     AS INT64                  NO-UNDO.
DEFINE VARIABLE macctnameyes   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE counter       AS INT64                  NO-UNDO.
DEFINE VARIABLE units         AS DECIMAL INITIAL 1        NO-UNDO.
DEFINE VARIABLE cols          AS INT64                  NO-UNDO.
DEFINE VARIABLE idx           AS INT64                  NO-UNDO.
DEFINE VARIABLE sdx           AS CHARACTER                NO-UNDO.
DEFINE VARIABLE j             AS INT64                  NO-UNDO.
DEFINE VARIABLE mj            AS INT64                  NO-UNDO.

DEFINE VARIABLE NAME AS CHARACTER EXTENT 10
   FORMAT "x(35)" COLUMN-LABEL "НАИМЕНОВАНИЕ" NO-UNDO.
DEFINE VARIABLE contract AS CHARACTER
   FORMAT "x(8)"  COLUMN-LABEL "НАЗНАЧЕНИЕ"   NO-UNDO.

DEFINE VARIABLE slash           AS CHARACTER FORMAT "x"                NO-UNDO.
DEFINE VARIABLE new-cur         LIKE op-entry.currency   INITIAL ""    NO-UNDO.
DEFINE VARIABLE aggr-type       AS CHARACTER FORMAT "x(16)" INITIAL "" NO-UNDO.
DEFINE VARIABLE ndays           AS INT64                             NO-UNDO.
DEFINE VARIABLE dispcols        AS CHARACTER INITIAL "acct,currency,"  NO-UNDO.
DEFINE VARIABLE header1         AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE header2         AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE header3         AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE tmplb           AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE one-string-name AS LOGICAL                             NO-UNDO.
DEFINE VARIABLE lname           AS INT64                             NO-UNDO.
DEFINE VARIABLE turnover        AS LOGICAL                             NO-UNDO.
DEFINE VARIABLE maxturn         AS LOGICAL                             NO-UNDO.
DEFINE VARIABLE NamePos         AS INT64                             NO-UNDO.
DEFINE VARIABLE CustPos         AS INT64                             NO-UNDO.
DEFINE VARIABLE xattr-length    AS INT64                             NO-UNDO.
DEFINE VARIABLE xattr-val       AS CHARACTER                           NO-UNDO.

DEFINE VARIABLE numformat       AS CHARACTER INITIAL "->>>,>>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE curformat       AS CHARACTER INITIAL "->>>,>>>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE zerospace       AS LOGICAL FORMAT "Да/Нет" INITIAL NO  NO-UNDO.
DEFINE VARIABLE zeroskip        AS LOGICAL FORMAT "Да/Нет" INITIAL YES NO-UNDO.
DEFINE VARIABLE thousands       AS LOGICAL                             NO-UNDO.
DEFINE VARIABLE mflag-zo        AS INT64                             NO-UNDO.
DEFINE VARIABLE mINN            AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE mSortName       AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE mSkipSort       AS LOGICAL                             NO-UNDO.

FUNCTION leftpad RETURNS CHAR (ch AS CHAR, l AS INT64):
   RETURN {leftpad.i ch " " l}.
END.

{headfoot.i &PROC=YES}

RUN genpfilt.p (INPUT-OUTPUT zerospace,
                INPUT-OUTPUT zeroskip,
                INPUT-OUTPUT mflag-zo,
                OUTPUT       turnover,
                OUTPUT       one-string-name,
                INPUT-OUTPUT aggr-type,
                OUTPUT       thousands,
                INPUT-OUTPUT dispcols,
                INPUT-OUTPUT vSortFlds,
                INPUT-OUTPUT vPosCurrency,
                INPUT-OUTPUT vPosBreaksrt,
                OUTPUT       vHEADFOOT_I_footer[1], 
                OUTPUT       vHEADFOOT_I_header[1],
                OUTPUT       mSkipSort
                ).

/* отказались */
IF RETURN-VALUE EQ "END-ERROR" THEN RETURN.
flag-ZO =      IF mflag-ZO EQ 1 THEN YES
          ELSE IF mflag-ZO EQ 2 THEN ?
          ELSE IF mflag-ZO EQ 3 THEN NO
          ELSE NO.

macctnameyes = (LOOKUP("name",dispcols) GT 0).

IF thousands THEN units = 1000.

IF turnover THEN DO:
   {getdates.i}
END.
ELSE DO:
   {getdate.i}
   beg-date = end-date.
END.

&IF DEFINED(persons) NE 0 &THEN
   {tempcust.i {&*}}
&ELSEIF DEFINED(recids) NE 0 &THEN
   {tempacct.i {&*}}
&ELSEIF DEFINED(nobalcur) = 0 &THEN
  DEF NEW SHARED VAR list-id AS CHAR FORMAT "X(40)" INITIAL "*" NO-UNDO.
  IF FGetSetting("Отчеты","ПоСотр","") EQ "Да" THEN DO:
      DO TRANSACTION:
         RUN op-user.p (6).
      END.
      IF KEYFUNC(LASTKEY) = "end-error" THEN UNDO, RETURN.
      IF NUM-ENTRIES(list-id) > 1 THEN access = "".
      ELSE access = list-id.
  END.
  ELSE list-id = "*".
  RELEASE _user.
  {getbac.i}
&ELSE
  DEF VAR list-id AS CHAR INITIAL "*" NO-UNDO.
&ENDIF

{justamin}
IF one-string-name EQ YES 
THEN lname = 80.
ELSE lname = 35.

RUN CalcData.
RUN PrepareHeader.
/* Сортировка */
RUN pSortData (vSortFlds).

/* Выгрузка   */
{setdest.i &cols=" + cols"}
{headfoot.i}

PUT UNFORMATTED {headfoot.i &head=YES} SKIP(1).
RUN PrintReport.
PUT UNFORMATTED SKIP(1) {headfoot.i &foot=YES} SKIP(1).
{signatur.i &user-only=YES}
ASSIGN
  access = ""
  flag-zo = NO.
{preview.i}

/************************************************************************/
PROCEDURE CalcData.
   DEF VAR d        AS DATE     NO-UNDO.
   DEF VAR tmp-db   LIKE sh-db  NO-UNDO.
   DEF VAR tmp-cr   LIKE sh-cr  NO-UNDO.
   DEF VAR tmp-vdb  LIKE sh-vdb NO-UNDO.
   DEF VAR tmp-vcr  LIKE sh-vcr NO-UNDO.
   DEF VAR tmp-qdb  LIKE sh-qdb NO-UNDO.
   DEF VAR tmp-qcr  LIKE sh-qcr NO-UNDO.
   DEF VAR vQtyNeed AS LOGICAL  NO-UNDO.
   DEF VAR vQDb     LIKE sh-qdb NO-UNDO.
   DEF VAR vQCr     LIKE sh-qcr NO-UNDO.
   DEF VAR vQty     LIKE sh-qty NO-UNDO.
   DEF VAR vRsrvNeed AS LOGICAL  NO-UNDO.
   DEF VAR vLastDate AS DATE     NO-UNDO.

   vQtyNeed = ( CAN-DO(dispcols,"val9") OR CAN-DO(dispcols,"val10") OR CAN-DO(dispcols,"val11") ).

   vRsrvNeed = ( CAN-DO(dispcols,"acct_rsrv") ).

   &IF DEFINED(recids) NE 0 OR DEFINED(persons) NE 0 &THEN
      FOR EACH tmprecid NO-LOCK,
         FIRST acct WHERE RECID(acct) = tmprecid.id
   &ELSE
      FOR EACH acct WHERE
      &IF DEFINED(nobalcur) EQ 0 &THEN
             CAN-DO(bac, STRING(acct.bal-acct))
         AND CAN-DO(cur, acct.currency)
         AND acct.acct-cat begins in-acct-cat AND
      &ENDIF
         (   acct.close-date EQ ?
          OR acct.close-date >= beg-date)
         AND acct.open-date <= end-date
         {&addwhere}
         AND acct.user-id BEGINS access
         AND CAN-DO(list-id,acct.user-id)
   &ENDIF
   NO-LOCK:

      IF aggr-type = "" THEN DO:
         IF vQtyNeed THEN DO:
            RUN acct-qty IN h_base (acct.acct, acct.currency, beg-date, end-date, ?).
            ASSIGN
               vQDb = sh-qdb
               vQCr = sh-qcr
               vQty = sh-qty
            .
         END.
         RUN acct-pos IN h_base (acct.acct, acct.currency, beg-date, end-date, ?).
      END.
      ELSE DO:
         IF vQtyNeed THEN DO:
            RUN aqty-sh.p (acct.acct, acct.currency, beg-date, end-date, ?).
            ASSIGN
               vQDb = sh-qdb
               vQCr = sh-qcr
               vQty = sh-qty
            .
            EMPTY TEMP-TABLE wrkqty.
            FOR EACH sh NO-LOCK:
               CREATE wrkqty.
               BUFFER-COPY sh USING since qdb qcr qty TO wrkqty.
            END.
         END.
         RUN apos-sh.p (acct.acct, acct.currency, beg-date, end-date, ?).
         IF vQtyNeed THEN
            FOR EACH sh,
               FIRST wrkqty WHERE wrkqty.since = sh.since NO-LOCK:
               BUFFER-COPY wrkqty USING qdb qcr qty TO sh.
            END.
      END.

      IF  (   zeroskip   = FALSE
           OR sh-in-bal <> 0
           OR sh-bal    <> 0
           OR sh-db     <> 0
           OR sh-cr     <> 0
           OR (    vQtyNeed = TRUE
               AND (   vQDb <> 0
                    OR vQCr <> 0
                    OR vQty <> 0)))
      AND NOT (    flag-ZO = ?
               AND sh-db   = 0
               AND sh-cr   = 0)
&IF DEFINED(skipzero) NE 0 &THEN
      AND NOT (    sh-in-bal = 0
               AND sh-bal    = 0                     /* если остатки на счете  = 0, */
               AND NOT (   CAN-DO(dispcols,"val5")   /* было движение на дату отчета, */
                        OR CAN-DO(dispcols,"val3")   /* но в отчете нет полей дб и кр */
                        OR CAN-DO(dispcols,"val6")   /* то эти счета не попадают в отчет */
                        OR CAN-DO(dispcols,"val4")))
&ENDIF
      THEN DO:
         CREATE wrkacct.
         ASSIGN
            wrkacct.bal-acct  = acct.bal-acct
            wrkacct.id        = RECID(acct)
            wrkacct.acct      = acct.acct
            wrkacct.currency = acct.currency
            wrkacct.breaksrt = STRING(acct.bal-acct,"99999")
            wrkacct.srtfield = SUBSTR(acct.acct,10) + acct.currency
         .

         RUN GetLinksTT(acct.Class-Code,
                        acct.acct + "," + acct.currency,
                        ?,
                        "acct-reserve",
                        end-date,
                        OUTPUT TABLE tt-linksur BY-REFERENCE).
         FIND FIRST tt-linksur NO-LOCK NO-ERROR.
         wrkacct.acct-rsrv = (IF AVAIL tt-linksur THEN tt-linksur.surrogate ELSE "").

         RUN GetCustName IN h_base (acct.cust-cat,acct.cust-id, acct.acct,
                                    OUTPUT wrkacct.cliname[1],
                                    OUTPUT wrkacct.cliname[2],
                                    INPUT-OUTPUT mINN).
         ASSIGN
            wrkacct.cliname[1] = wrkacct.cliname[1] + " " + wrkacct.cliname[2]
            wrkacct.cliname[2] = "".
         .

         IF macctnameyes THEN
         DO:
            {getcust.i &name="name" &OFFinn="/*"} 
            wrkacct.acctname = trim(name[1] + " " + name[2]).
         END.
         CASE aggr-type:
            WHEN "" THEN ASSIGN
               wrkacct.val[1]  = sh-bal
               wrkacct.val[2]  = sh-val
               wrkacct.val[3]  = sh-db
               wrkacct.val[4]  = sh-cr
               wrkacct.val[5]  = sh-vdb
               wrkacct.val[6]  = sh-vcr
               wrkacct.val[7]  = sh-in-bal
               wrkacct.val[8]  = sh-in-val
               wrkacct.val[9]  = vQDb
               wrkacct.val[10] = vQCr
               wrkacct.val[11] = vQty
               wrkacct.since = if acct.currency = "" THEN lastmove ELSE lastcurr
            .
            WHEN "среднее" OR
            WHEN "ср. хронолог." THEN DO:
               ndays = 0.
               wrkacct.val = 0.
               DO d = beg-date TO end-date:
                  FIND LAST sh WHERE sh.since <= d NO-ERROR.
                  IF AVAIL sh THEN DO:
                     ASSIGN
                        tmp-db  = (IF sh.since = d THEN sh.db  ELSE 0)
                        tmp-cr  = (IF sh.since = d THEN sh.cr  ELSE 0)
                        tmp-vdb = (IF sh.since = d THEN sh.vdb ELSE 0)
                        tmp-vcr = (IF sh.since = d THEN sh.vcr ELSE 0)
                        tmp-qdb = (IF sh.since = d THEN sh.qdb ELSE 0)
                        tmp-qcr = (IF sh.since = d THEN sh.qcr ELSE 0)
                     .
                     IF  (   d = beg-date
                          OR d = end-date)
                     AND aggr-type = "ср. хронолог."
                     AND beg-date <> end-date THEN
                        ASSIGN
                           wrkacct.val[1]  = wrkacct.val[1]  + sh.bal / 2
                           wrkacct.val[2]  = wrkacct.val[2]  + sh.val / 2
                           wrkacct.val[3]  = wrkacct.val[3]  + tmp-db / 2
                           wrkacct.val[4]  = wrkacct.val[4]  + tmp-cr / 2
                           wrkacct.val[5]  = wrkacct.val[5]  + tmp-vdb / 2
                           wrkacct.val[6]  = wrkacct.val[6]  + tmp-vcr / 2
                           wrkacct.val[7]  = wrkacct.val[7]  + (sh.bal - tmp-db  + tmp-cr)  / 2
                           wrkacct.val[8]  = wrkacct.val[8]  + (sh.val - tmp-vdb + tmp-vcr) / 2
                           wrkacct.val[9]  = wrkacct.val[9]  + tmp-qdb / 2
                           wrkacct.val[10] = wrkacct.val[10] + tmp-qcr / 2
                           wrkacct.val[11] = wrkacct.val[11] + sh.qty / 2
                           wrkacct.since   = sh.since
                        .
                     ELSE
                        ASSIGN
                           wrkacct.val[1]  = wrkacct.val[1]  + sh.bal
                           wrkacct.val[2]  = wrkacct.val[2]  + sh.val
                           wrkacct.val[3]  = wrkacct.val[3]  + tmp-db
                           wrkacct.val[4]  = wrkacct.val[4]  + tmp-cr
                           wrkacct.val[5]  = wrkacct.val[5]  + tmp-vdb
                           wrkacct.val[6]  = wrkacct.val[6]  + tmp-vcr
                           wrkacct.val[7]  = wrkacct.val[7]  + sh.bal - tmp-db + tmp-cr
                           wrkacct.val[8]  = wrkacct.val[8]  + sh.val - tmp-vdb + tmp-vcr
                           wrkacct.val[9]  = wrkacct.val[9]  + tmp-qdb
                           wrkacct.val[10] = wrkacct.val[10] + tmp-qcr
                           wrkacct.val[11] = wrkacct.val[11] + sh.qty
                           wrkacct.since   = sh.since
                        .
                  END.
                  ndays = ndays + 1.
               END.
               IF  aggr-type = "ср. хронолог."
               AND beg-date <> end-date THEN
                  ndays = ndays - 1.
               DO j = 1 TO 12:
                  wrkacct.val[j] = wrkacct.val[j] / ndays.
               END.
            END.
            WHEN "минимум" THEN DO:
               FIND LAST sh WHERE sh.since < beg-date NO-ERROR.
               IF AVAIL sh THEN vLastDate = sh.since.
               ELSE vLastDate = beg-date.
               wrkacct.val = 1000000000000000000.0.
               FOR EACH sh WHERE sh.since >= vLastDate:
                  ASSIGN
                     wrkacct.val[1]  = sh.bal WHEN ABS(wrkacct.val[1]) > ABS(sh.bal)
                     wrkacct.val[2]  = sh.val WHEN ABS(wrkacct.val[2]) > ABS(sh.val)
                     wrkacct.val[3]  = sh.db  WHEN wrkacct.val[3] > sh.db
                     wrkacct.val[4]  = sh.cr  WHEN wrkacct.val[4] > sh.cr
                     wrkacct.val[5]  = sh.vdb WHEN wrkacct.val[5] > sh.vdb
                     wrkacct.val[6]  = sh.vcr WHEN wrkacct.val[6] > sh.vcr
                     wrkacct.val[7]  = sh.bal - sh.db  + sh.cr  WHEN ABS(wrkacct.val[7]) > ABS(sh.bal - sh.db  + sh.cr)
                     wrkacct.val[7]  = sh.bal WHEN sh.since < end-date AND ABS(wrkacct.val[7]) > ABS(sh.bal)
                     wrkacct.val[8]  = sh.val - sh.vdb + sh.vcr WHEN ABS(wrkacct.val[8]) > ABS(sh.val - sh.vdb + sh.vcr)
                     wrkacct.val[8]  = sh.val WHEN sh.since < end-date AND ABS(wrkacct.val[8]) > ABS(sh.val)
                     wrkacct.val[9]  = sh.qdb WHEN wrkacct.val[9]  > sh.qdb
                     wrkacct.val[10] = sh.qcr WHEN wrkacct.val[10] > sh.qcr
                     wrkacct.val[11] = sh.qty WHEN wrkacct.val[11] > sh.qty
                     wrkacct.since   = sh.since
                  .
               END.
               DO j = 1 TO 12:
                  IF wrkacct.val[j] = 1000000000000000000.0 THEN
                     wrkacct.val[j] = 0.
               END.
            END.
            WHEN "максимум" THEN DO:
               IF NOT CAN-FIND(FIRST sh WHERE sh.since = beg-date) THEN DO:
                  FIND FIRST sh WHERE sh.since = beg-date - 1 NO-ERROR.
                  IF AVAIL sh THEN sh.since = beg-date.
               END.
               wrkacct.val = 0.
               FOR EACH sh WHERE sh.since >= beg-date:
                  ASSIGN
                     wrkacct.val[1]  = sh.bal WHEN ABS(wrkacct.val[1]) < ABS(sh.bal)
                     wrkacct.val[2]  = sh.val WHEN ABS(wrkacct.val[2]) < ABS(sh.val)
                     wrkacct.val[3]  = sh.db  WHEN wrkacct.val[3] < sh.db
                     wrkacct.val[4]  = sh.cr  WHEN wrkacct.val[4] < sh.cr
                     wrkacct.val[5]  = sh.vdb WHEN wrkacct.val[5] < sh.vdb
                     wrkacct.val[6]  = sh.vcr WHEN wrkacct.val[6] < sh.vcr
                     wrkacct.val[7]  = sh.bal - sh.db  + sh.cr  WHEN ABS(wrkacct.val[7]) < ABS(sh.bal - sh.db  + sh.cr)
                     wrkacct.val[7]  = sh.bal WHEN sh.since < end-date AND ABS(wrkacct.val[7]) < ABS(sh.bal)
                     wrkacct.val[8]  = sh.val - sh.vdb + sh.vcr WHEN ABS(wrkacct.val[8]) < ABS(sh.val - sh.vdb + sh.vcr)
                     wrkacct.val[8]  = sh.val WHEN sh.since < end-date AND ABS(wrkacct.val[8]) < ABS(sh.val)
                     wrkacct.val[9]  = sh.qdb WHEN wrkacct.val[9] < sh.qdb
                     wrkacct.val[10] = sh.qcr WHEN wrkacct.val[10] < sh.qcr
                     wrkacct.val[11] = sh.qty WHEN wrkacct.val[11] < sh.qty
                     wrkacct.since   = sh.since
                  .
               END.
            END.
         END CASE. /* case */

         wrkacct.cont-code = "".
         _sch_la:
         FOR EACH loan-acct WHERE loan-acct.acct     EQ acct.acct
                              AND loan-acct.currency EQ acct.currency
         NO-LOCK,
         FIRST loan OF loan-acct
         NO-LOCK
         BY loan.contract = "кредит" DESCENDING BY loan-acct.since DESCENDING:
            IF NOT {assigned wrkacct.cont-code}
            THEN wrkacct.cont-code = loan.doc-ref.
            ELSE DO:
               wrkacct.cont-code = "*" + wrkacct.cont-code.
               LEAVE _sch_la.
            END.
         END.

      END.
   END.
END PROCEDURE.

PROCEDURE PrepareHeader.

   DEF VAR fl AS LOGICAL NO-UNDO.

/*        добавляем   код клиента  */

dispcols = dispcols + ",cust-id".


   DO j = 1 TO NUM-ENTRIES(dispcols):
      CASE ENTRY(j,dispcols):
         WHEN "acct" THEN ASSIGN
            header1 = header1 + " " + STRING("СЧЕТ","x(25)")
            header2 = header2 + " " + FILL("-",25)
            header3 = header3 + " " + FILL(" ",25)
         .
         WHEN "currency" THEN ASSIGN
            header1 = header1 + " ВАЛ"
            header2 = header2 + " ---"
            header3 = header3 + "    "
         .
         WHEN "custname" THEN ASSIGN
            header1 = header1 + " " + STRING("НАИМЕНОВАНИЕ КЛИЕНТА","x(" + STRING(lname) + ")")
            header2 = header2 + " " + FILL("-",IF one-string-name THEN 80 ELSE 35)
            header3 = header3 + " " + FILL(" ",IF one-string-name THEN 80 ELSE 35)
         .
         WHEN "cust-id" THEN ASSIGN
            header1 = header1 + " " + STRING("КОД КЛ.","x(7)")
            header2 = header2 + " " + FILL("-",10)
            header3 = header3 + " " + FILL(" ",10)
         .
         WHEN "name" THEN ASSIGN
            header1 = header1 + " " + STRING("НАИМЕНОВАНИЕ СЧЕТА","x(" + STRING(lname) + ")")
            header2 = header2 + " " + FILL("-",IF one-string-name THEN 80 ELSE 35)
            header3 = header3 + " " + FILL(" ",IF one-string-name THEN 80 ELSE 35)
         .
         WHEN "contract" THEN ASSIGN
            header1 = header1 + " " + STRING("НАЗНАЧЕНИЕ","x(10)")
            header2 = header2 + " " + FILL("-",10)
            header3 = header3 + " " + FILL(" ",10)
         .
         WHEN "side" THEN ASSIGN
            header1 = header1 + STRING(" А/П","x(4)")
            header2 = header2 + " ---"
            header3 = header3 + "    "
         .
         WHEN "since" THEN ASSIGN
            header1 = header1 + "        ДПД"
            header2 = header2 + " ----------"
            header3 = header3 + "           "
         .
         WHEN "dubl_acct" THEN ASSIGN
            header1 = header1 + " " + STRING("ПАРНЫЙ СЧЕТ","x(25)")
            header2 = header2 + " " + FILL("-",25)
            header3 = header3 + " " + FILL(" ",25)
         .
         WHEN "d_open" THEN ASSIGN
            header1 = header1 + " ДАТА ОТКР."
            header2 = header2 + " ----------"
            header3 = header3 + "           "
         .
         WHEN "d_close" THEN ASSIGN
            header1 = header1 + " ДАТА ЗАКР."
            header2 = header2 + " ----------"
            header3 = header3 + "           "
         .
         WHEN "t_curr" THEN ASSIGN
            header1 = header1 + "  ТИП КУРСА  "
            header2 = header2 + " ------------"
            header3 = header3 + "             "
         .
         WHEN "val1" OR WHEN "val2" OR WHEN "val3" OR WHEN "val4" or
         WHEN "val5" OR WHEN "val6" OR WHEN "val7" OR WHEN "val8" or
         WHEN "val9" OR WHEN "val10" OR WHEN "val11" OR WHEN "val12" THEN do:
            sdx = substr(ENTRY(j,dispcols),4).
            tmplb = if lookup(sdx,"1,2,7,8,11") > 0 THEN "ОСТАТОК"
                    ELSE if lookup(sdx,"3,5,9") > 0 THEN "ДЕБЕТ"
                    ELSE if lookup(sdx,"4,6,10") > 0 THEN "КРЕДИТ" ELSE "".
            if lookup(sdx,"2,5,6,8") > 0 then
               tmplb = tmplb + " В ИНВ.".
            if lookup(sdx,"1,2") > 0 then
               tmplb = "ИСХ. " + tmplb.
            ELSE if lookup(sdx,"7,8") > 0 then
               tmplb = "ВХ. " + tmplb.
            ELSE if lookup(sdx,"9,10,11") > 0 then
               tmplb = tmplb + " КОЛ-ВА".
            if lookup(sdx,"3,4,5,6,11") = 0 THEN ASSIGN
               header1 = header1 + " " + leftpad(tmplb,LENGTH(numformat) + 3)
               header2 = header2 + " " + FILL("-",LENGTH(numformat) + 3)
               header3 = header3 + " " + FILL("-",LENGTH(numformat) + 3).
            ELSE ASSIGN
               header1 = header1 + " " + leftpad(tmplb,LENGTH(numformat))
               header2 = header2 + " " + FILL("-",LENGTH(numformat))
               header3 = header3 + " " + FILL("-",LENGTH(numformat)).
         END.
         WHEN "nloan" THEN
         DO:
            ASSIGN
               header1 = header1 + "           НОМЕР ДОГОВОРА           "
               header2 = header2 + " -----------------------------------"
               header3 = header3 + "                                    "
            .
         END.
         WHEN "acct_rsrv" THEN
            ASSIGN
               header1 = header1 + " " + STRING("СЧЕТ РЕЗЕРВА","x(25)")
               header2 = header2 + " " + FILL("-",25)
               header3 = header3 + " " + FILL(" ",25)
           .
                     /* Значит, допреквизит */
         OTHERWISE DO:
            FIND FIRST xattr WHERE
                       xattr.xattr-code = SUBSTR(ENTRY(j,dispcols),4)
                   AND xattr.class-code BEGINS "acct"
            NO-LOCK NO-ERROR.
            FIND FIRST xattr-temp WHERE xattr-temp.xattr-code = xattr.xattr-code NO-ERROR.
            IF NOT AVAIL xattr-temp THEN DO:
               CREATE xattr-temp.
               ASSIGN xattr-temp.xattr-code = TRIM(xattr.xattr-code)
                      xattr-temp.data-format = xattr.data-format.
               ASSIGN
                   xattr-length = LENGTH(STRING(FILL(" ",300),xattr.data-format))
                   fl = LENGTH(xattr.name) gt xattr-length
                   xattr-length = max(LENGTH(if fl THEN xattr.xattr-code ELSE xattr.name), xattr-length)
                   xattr-length = min(30,xattr-length)
                   header1 = header1 + " " + STRING(caps(if fl THEN xattr.xattr-code ELSE xattr.name),FILL("x",xattr-length))
                   header2 = header2 + " " + FILL("-",xattr-length)
                   header3 = header3 + " " + FILL(" ",xattr-length)
                   xattr-temp.LENGTH = xattr-length NO-ERROR.
               ASSIGN xattr-temp.data-type = xattr.data-type.
            END.
         END.
      END CASE.
   END.
   ASSIGN
      cols    = LENGTH(header2)
      NamePos = INDEX(header1,"НАИМЕНОВАНИЕ СЧЕТА")
      CustPos = INDEX(header1,"НАИМЕНОВАНИЕ КЛИЕНТА")
   .

END PROCEDURE.

PROCEDURE PrintReport.

   DEF VAR tophead AS CHAR    NO-UNDO.
   DEF VAR bal     AS DECIMAL NO-UNDO.

   DEFINE VARIABLE mIsFirst AS LOGICAL INITIAL YES NO-UNDO.

   tophead =
      (IF turnover THEN "ОБОРОТНО-САЛЬДОВАЯ ВЕДОМОСТЬ ЗА "
                   ELSE "ВЕДОМОСТЬ ОСТАТКОВ ЗА ")
      + CAPS({term2str Beg-Date End-Date})
      + (IF flag-zo THEN " (ВКЛЮЧАЯ ЗО)" ELSE IF flag-zo = ? THEN " (ТОЛЬКО ЗО)" ELSE "")
      + (IF thousands THEN " ({&in-UA-1000NCN})" ELSE "")
      + (IF aggr-type = "" THEN "" ELSE " (" +
      ENTRY(lookup(aggr-type, "ср. хронолог.,среднее,минимум,максимум"),
      "СРЕДНИЕ ХРОНОЛОГИЧЕСКИЕ,СРЕДНИЕ,МИНИМАЛЬНЫЕ,МАКСИМАЛЬНЫЕ")
      + " ЗНАЧЕНИЯ)")
      .
   tophead = tophead + FILL(" ", LENGTH(header1) - LENGTH(tophead) - 10) + "Лист".

   if page-size = 0 then
   DO:
      mIsFirst = NO.
      PUT UNFORMATTED tophead STRING(1,">>>>>9") skip(1)
          header1 skip header2 skip.
   END.

   FOR EACH ttSort USE-INDEX idxSort,
      FIRST wrkacct WHERE ROWID(wrkacct) EQ ttSort.Rwd NO-LOCK,
      FIRST acct WHERE    RECID(acct)    EQ wrkacct.id NO-LOCK
      BREAK BY ttSort.Sort1
            BY ttSort.Sort2
            BY ttSort.Sort3
            BY ttSort.Sort4
            BY ttSort.Sort5:

      {chkpage IF (LAST-OF(ttSort.Sort1) AND vPosBreaksrt EQ 1)
               OR (LAST-OF(ttSort.Sort2) AND vPosBreaksrt EQ 2)
               OR (LAST-OF(ttSort.Sort3) AND vPosBreaksrt EQ 3)
               OR (LAST-OF(ttSort.Sort4) AND vPosBreaksrt EQ 4)
               OR (LAST-OF(ttSort.Sort5) AND vPosBreaksrt EQ 5) THEN 3 ELSE 1}

      IF page-size > 0 AND (line-counter = 1 OR mIsFirst) THEN
      DO:
         mIsFirst = NO.
         PUT UNFORMATTED tophead STRING(page-number,">>>>>9") skip(1)
             header1 skip header2 skip.
      END.

      &IF DEFINED(recids) NE 0 OR DEFINED(persons) NE 0 OR DEFINED(nobalcur) NE 0 &THEN
         {get-fmt.i &obj='" + acct.acct-cat + ""-Acct-Fmt"" + "'}
      &ENDIF
      
      DO j = 1 TO NUM-ENTRIES(dispcols):
         CASE ENTRY(j,dispcols):
            WHEN "acct" THEN DO:
               PUT UNFORMATTED " " STRING({out-fmt.i wrkacct.acct fmt},"x(25)").

            END.
            WHEN "currency" THEN do:
               run currcode in h_base (wrkacct.currency, output new-cur).
               PUT UNFORMATTED " " STRING(wrkacct.currency,"xxx").

            END.
            WHEN "cust-id" THEN do:
               PUT UNFORMATTED " " acct.cust-id.

            END.
            WHEN "side" THEN do:
               PUT UNFORMATTED " " STRING(acct.side," xx").

            END.
            WHEN "d_open" THEN do:
               PUT UNFORMATTED " " STRING(acct.open-date,"99/99/9999").

            END.
            WHEN "d_close" THEN do:
               PUT UNFORMATTED " " if acct.close-date = ? THEN "          "
                                   ELSE STRING(acct.close-date,"99/99/9999").

            END.
            WHEN "dubl_acct" THEN do:
               PUT UNFORMATTED " " STRING(acct.contr-acct,"x(25)").

            END.
            WHEN "t_curr" THEN do:
               PUT UNFORMATTED " " STRING(acct.rate-type,"x(12)").

            END.
            WHEN "since" THEN do:
               PUT UNFORMATTED " " if wrkacct.since = ? THEN "          "
                                   ELSE STRING(wrkacct.since,"99/99/9999").

            END.
            WHEN "name"THEN
            DO:
               name[1] = wrkacct.acctname.
               {wordwrap.i &s=name &l=lname &n=10}
               PUT UNFORMATTED " " STRING(name[1],"x(" + STRING(lname) + ")") AT NamePos.
            END.
            WHEN "custname" THEN
            DO:
               {wordwrap.i &s=wrkacct.cliname &l=lname &n=10}
               PUT UNFORMATTED " " STRING(wrkacct.cliname[1],"x(" + STRING(lname) + ")") AT CustPos.
            END.
            WHEN "contract" THEN do:
               PUT UNFORMATTED " " STRING(acct.contract,"x(10)").
            END.
            WHEN "val1" OR WHEN "val2" OR WHEN "val3" OR WHEN "val4" or
            WHEN "val5" OR WHEN "val6" OR WHEN "val7" OR WHEN "val8" or
            WHEN "val9" OR WHEN "val10" OR WHEN "val11" OR WHEN "val12" THEN do:
               idx = INT64(SUBSTR(ENTRY(j,dispcols),4)).
               RUN putnumber (wrkacct.val[idx]).
            END.
            WHEN "nloan" THEN
            DO:
               PUT UNFORMATTED " " STRING(wrkacct.cont-code, "x(35)").
            END.
            WHEN "acct_rsrv" THEN 
               PUT UNFORMATTED " " STRING(STRING(wrkacct.acct-rsrv,fmt),"x(25)").
            /* Значит, допреквизит */
            OTHERWISE DO:
               FIND FIRST xattr-temp WHERE
                          xattr-temp.xattr-code = SUBSTR(ENTRY(j,dispcols),4)
               NO-LOCK NO-ERROR.

               IF IsTemporal("acct", xattr-temp.xattr-code) THEN
                  xattr-val = GetTempXAttrValueEx("acct", acct.acct + "," + acct.currency, xattr-temp.xattr-code, End-Date, "").
               ELSE
                  xattr-val = GetXAttrValue("acct",acct.acct + "," + acct.currency, xattr-temp.xattr-code).

               IF xattr-val = "" THEN
                  PUT UNFORMATTED FILL(" ",xattr-temp.LENGTH + 1).
               ELSE do:
                  if(xattr-temp.data-type = "date") then
                    PUT UNFORMATTED " " STRING(date(xattr-val),xattr-temp.data-format) format "x(" + STRING(xattr-temp.LENGTH) + ")".
                  ELSE if(xattr-temp.data-type = "logical") then
                    PUT UNFORMATTED " " STRING( if (lookup(xattr-val,xattr-temp.data-format,"/") NE 0 )~
                                                THEN ENTRY(lookup(xattr-val,xattr-temp.data-format,"/"),xattr-temp.data-format,"/")~
                                                ELSE "" )
                    format "x(" + STRING(xattr-temp.LENGTH) + ")".
                  ELSE IF (xattr-temp.data-type = "DECIMAL") THEN
                     PUT UNFORMATTED " " STRING( DECIMAL(xattr-val),xattr-temp.data-format) 
                        FORMAT "x(" + STRING(xattr-temp.LENGTH) + ")".
                  ELSE IF (xattr-temp.data-type = "INTEGER") THEN
                     PUT UNFORMATTED " " STRING( INT64(xattr-val),xattr-temp.data-format) 
                        FORMAT "x(" + STRING(xattr-temp.LENGTH) + ")".
                  ELSE
                    PUT UNFORMATTED " " STRING(xattr-val ,xattr-temp.data-format) format "x(" + STRING(xattr-temp.LENGTH) + ")".
               END.
            END.
         END CASE.
      END.
      IF NOT one-string-name THEN 
      DO MJ = 2 TO 10:
         IF name[mj] <> "" OR wrkacct.cliname[mj] <> "" THEN DO:
            PUT SKIP.
            IF LOOKUP("name",dispcols) NE 0 
            AND LOOKUP("name",dispcols) < LOOKUP("custname",dispcols) THEN
               PUT UNFORMATTED STRING(name[mj],"x(" + STRING(lname) + ")") AT NamePos.
            IF LOOKUP("custname",dispcols) NE 0 THEN
               PUT UNFORMATTED STRING(wrkacct.cliname[mj],"x(" + STRING(lname) + ")") AT CustPos.
            IF LOOKUP("name",dispcols) NE 0 
            AND LOOKUP("name",dispcols) > LOOKUP("custname",dispcols) THEN
               PUT UNFORMATTED STRING(name[mj],"x(" + STRING(lname) + ")") AT NamePos.
          END.
       END.

      ACCUMULATE
         wrkacct.val[1]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[2]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[3]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[4]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[5]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[6]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[7]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[8]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[9]  (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[10] (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
         wrkacct.val[11] (TOTAL BY ttSort.Sort1 BY ttSort.Sort2 BY ttSort.Sort3 BY ttSort.Sort4 BY ttSort.Sort5)
      .
      PUT SKIP.
      IF mSkipSort THEN DO:
         IF LAST-OF(ttSort.Sort1) THEN DO:
            PUT UNFORMATTED SKIP header3 SKIP.
            DO j = 1 TO NUM-ENTRIES ( dispcols ):
               CASE ENTRY ( j, dispcols ):
                  WHEN "acct"      THEN PUT UNFORMATTED FILL(" ",26).
                  WHEN "currency"  THEN PUT UNFORMATTED " " STRING ( wrkacct.currency, "x(3)" ).
                  WHEN "since"     THEN PUT UNFORMATTED " " FILL(" ",10).
                  WHEN "side"      THEN PUT UNFORMATTED "    ".
                  WHEN "t_curr"    THEN PUT UNFORMATTED " " FILL(" ",12).
                  WHEN "custname"  THEN PUT UNFORMATTED " " FILL(" ",35).
                  WHEN "name"      THEN PUT UNFORMATTED " " STRING ( "Итого по " + ttSort.SortName, "x(" + STRING(lname) + ")" ). 
                  WHEN "contract"  THEN PUT UNFORMATTED " " FILL(" ",10).
                  WHEN "dubl_acct" THEN PUT UNFORMATTED " " FILL(" ",25).
                  WHEN "d_close" OR
                  WHEN "d_open"     THEN PUT UNFORMATTED " " FILL(" ",9).
                  WHEN "val1"    or
                  WHEN "val2"    or
                  WHEN "val3"    or
                  WHEN "val4"    or
                  WHEN "val5"    or
                  WHEN "val6"    or
                  WHEN "val7"    or
                  WHEN "val8"    OR
                  WHEN "val9"    OR
                  WHEN "val10"   OR
                  WHEN "val11"   THEN DO:
                     idx = INT64 ( SUBSTR ( ENTRY ( j, dispcols ), 4 ) ).
                     case idx:
                        WHEN 1 THEN 
                        DO: 
                           {genpotot.i 1}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 2 THEN 
                        DO: 
                           {genpotot.i 2}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 3 THEN 
                        DO: 
                           {genpotot.i 3}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 4 THEN 
                        DO: 
                           {genpotot.i 4}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 5 THEN 
                        DO: 
                           {genpotot.i 5}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 6 THEN 
                        DO: 
                           {genpotot.i 6}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 7 THEN 
                        DO: 
                           {genpotot.i 7}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 8 THEN 
                        DO: 
                           {genpotot.i 8}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 9 THEN 
                        DO: 
                           {genpotot.i 9}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 10 THEN 
                        DO: 
                           {genpotot.i 10}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                        WHEN 11 THEN 
                        DO: 
                           {genpotot.i 11}
                           RUN putnumber (vCurrentTotal[1]).
                        END.
                     END.
                  END.
                  WHEN "nloan" THEN
                  DO:
                     PUT UNFORMATTED " " SPACE(35).
                  END.
                  WHEN "acct_rsrv" THEN PUT UNFORMATTED FILL(" ",26).
                  OTHERWISE DO:
                     FIND FIRST xattr-temp WHERE
                                xattr-temp.xattr-code = SUBSTR(ENTRY(j,dispcols),4,-1)
                     NO-ERROR.
                     PUT UNFORMATTED " " FILL(" ",xattr-temp.LENGTH).
                  END.
               END CASE.
            END.
            PUT SKIP (1).
         END.
         IF    (LAST(ttSort.Sort1) AND vPosBreaksrt EQ 1)
            OR (LAST(ttSort.Sort2) AND vPosBreaksrt EQ 2)
            OR (LAST(ttSort.Sort3) AND vPosBreaksrt EQ 3)
            OR (LAST(ttSort.Sort4) AND vPosBreaksrt EQ 4)
            OR (LAST(ttSort.Sort5) AND vPosBreaksrt EQ 5) THEN 
         DO:
            PUT UNFORMATTED header3 skip.
            do j = 1 to num-entries(dispcols):
               case ENTRY(j,dispcols):
                  WHEN "acct"       THEN PUT UNFORMATTED FILL(" ",26).
                  WHEN "currency"   THEN PUT UNFORMATTED " " FILL(" ",3).
                  WHEN "since"      THEN PUT UNFORMATTED " " FILL(" ",10).
                  WHEN "side"       THEN PUT UNFORMATTED "    ".
                  WHEN "t_curr"     THEN PUT UNFORMATTED " " FILL(" ",12).
                  WHEN "custname"   THEN PUT UNFORMATTED " " FILL(" ",35).
                  WHEN "name"       THEN PUT UNFORMATTED " " STRING ( "Итого по отчету", "x(" + STRING(lname) + ")" ).
                  WHEN "contract"   THEN PUT UNFORMATTED " " FILL(" ",10).
                  WHEN "dubl_acct"  THEN PUT UNFORMATTED " " FILL(" ",25).
                  WHEN "d_close" OR
                  WHEN "d_open"     THEN PUT UNFORMATTED " " FILL(" ",9).
                  WHEN "val1"    OR
                  WHEN "val3"    OR
                  WHEN "val4"    OR
                  WHEN "val7"    OR
                  WHEN "val9"    OR
                  WHEN "val10"   OR
                  WHEN "val11"   THEN DO:
                     idx = INT64(substr(ENTRY(j,dispcols),4)).
                     CASE idx:
                        WHEN 1 THEN
                        DO:
                           {genpotot.i 1}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 3 THEN
                        DO:
                           {genpotot.i 3}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 4 THEN
                        DO:
                           {genpotot.i 4}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 7 THEN
                        DO:
                           {genpotot.i 7}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 9 THEN
                        DO:
                           {genpotot.i 9}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 10 THEN
                        DO:
                           {genpotot.i 10}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 11 THEN
                        DO:
                           {genpotot.i 11}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                     END.
                  END.
                  WHEN "val2" OR WHEN "val5" OR WHEN "val6" OR WHEN "val8" THEN do:
                     idx = INT64 ( SUBSTR ( ENTRY ( j, dispcols ),4 ) ).
                     RUN putnumber ( ? ).
                  END.
                  WHEN "nloan" THEN
                  DO:
                     PUT UNFORMATTED " " SPACE(35).
                  END.
                  WHEN "acct_rsrv" THEN PUT UNFORMATTED FILL(" ",26).
                  OTHERWISE DO:
                     FIND FIRST xattr-temp WHERE
                                xattr-temp.xattr-code = SUBSTR(ENTRY(j,dispcols),4,-1)
                     NO-ERROR.
                     PUT UNFORMATTED " " FILL(" ",xattr-temp.LENGTH).
                  END.
               END CASE.
            END.
         END.
      END.
      ELSE DO:
      /* Вывод подитогов - по валюте */
      IF    (LAST-OF(ttSort.Sort1) AND vPosCurrency EQ 1)
         OR (LAST-OF(ttSort.Sort2) AND vPosCurrency EQ 2)
         OR (LAST-OF(ttSort.Sort3) AND vPosCurrency EQ 3)
         OR (LAST-OF(ttSort.Sort4) AND vPosCurrency EQ 4)
         OR (LAST-OF(ttSort.Sort5) AND vPosCurrency EQ 5) THEN 
      DO: 
         
         PUT UNFORMATTED SKIP header3 SKIP.
         DO j = 1 TO NUM-ENTRIES ( dispcols ):
            CASE ENTRY ( j, dispcols ):
               WHEN "acct"       THEN PUT UNFORMATTED FILL(" ",26).
               WHEN "currency"   THEN PUT UNFORMATTED " " STRING ( wrkacct.currency, "x(3)" ).
               WHEN "since"      THEN PUT UNFORMATTED " " FILL(" ",10).
               WHEN "side"       THEN PUT UNFORMATTED "    ".
               WHEN "t_curr"     THEN PUT UNFORMATTED " " FILL(" ",12).
               WHEN "custname"   THEN PUT UNFORMATTED " " FILL(" ",35).
               WHEN "name"       THEN PUT UNFORMATTED " " STRING ( "Итого по валюте", "x(" + STRING(lname) + ")" ). 
               WHEN "contract"   THEN PUT UNFORMATTED " " FILL(" ",10).
               WHEN "dubl_acct"  THEN PUT UNFORMATTED " " FILL(" ",25).
               WHEN "d_close" OR
               WHEN "d_open"     THEN PUT UNFORMATTED " " FILL(" ",9).
               WHEN "val1"    OR
               WHEN "val2"    OR
               WHEN "val3"    OR
               WHEN "val4"    OR
               WHEN "val5"    OR
               WHEN "val6"    OR
               WHEN "val7"    OR
               WHEN "val8"    OR
               WHEN "val9"    OR
               WHEN "val10"   OR
               WHEN "val11"   THEN DO:
                  idx = INT64(SUBSTR(ENTRY(j, dispcols ), 4 )).
                  CASE idx:
                     WHEN 1 THEN 
                     DO: 
                        {genpotot.i 1}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 2 THEN 
                     DO: 
                        {genpotot.i 2}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 3 THEN 
                     DO: 
                        {genpotot.i 3}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 4 THEN 
                     DO: 
                        {genpotot.i 4}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 5 THEN 
                     DO: 
                        {genpotot.i 5}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 6 THEN 
                     DO: 
                        {genpotot.i 6}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 7 THEN 
                     DO: 
                        {genpotot.i 7}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 8 THEN 
                     DO: 
                        {genpotot.i 8}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 9 THEN 
                     DO: 
                        {genpotot.i 9}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 10 THEN 
                     DO: 
                        {genpotot.i 10}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                     WHEN 11 THEN 
                     DO: 
                        {genpotot.i 11}
                        RUN putnumber (vCurrentTotal[vPosCurrency]).
                     END.
                  END.
               END.
               WHEN "nloan" THEN
               DO:
                  PUT UNFORMATTED " " SPACE(35).
               END.
               WHEN "acct_rsrv" THEN PUT UNFORMATTED FILL(" ",26).
               OTHERWISE DO:
                  FIND FIRST xattr-temp WHERE
                             xattr-temp.xattr-code = SUBSTR(ENTRY(j,dispcols),4,-1)
                  NO-ERROR.
                  PUT UNFORMATTED " " FILL(" ",xattr-temp.LENGTH).
               END.
            END CASE.
         END.
         PUT SKIP (1).
      END.
      /* Вывод подитогов - по БС */
      IF    (LAST-OF(ttSort.Sort1) AND vPosBreaksrt EQ 1)
         OR (LAST-OF(ttSort.Sort2) AND vPosBreaksrt EQ 2)
         OR (LAST-OF(ttSort.Sort3) AND vPosBreaksrt EQ 3)
         OR (LAST-OF(ttSort.Sort4) AND vPosBreaksrt EQ 4)
         OR (LAST-OF(ttSort.Sort5) AND vPosBreaksrt EQ 5) THEN 
      DO:
         PUT UNFORMATTED header3 SKIP.
         DO j = 1 TO NUM-ENTRIES ( dispcols ):
            CASE ENTRY ( j, dispcols ):
               WHEN "acct"       THEN PUT UNFORMATTED FILL(" ",26).
               WHEN "currency"   THEN PUT UNFORMATTED " " FILL(" ",3).
               WHEN "since"      THEN PUT UNFORMATTED " " FILL(" ",10).
               WHEN "side"       THEN PUT UNFORMATTED "    ".
               WHEN "t_curr"     THEN PUT UNFORMATTED " " FILL(" ",12).
               WHEN "custname"   THEN PUT UNFORMATTED " " FILL(" ",35).
               WHEN "name"       THEN PUT UNFORMATTED " " STRING ( "Итого по балансовому счету " + wrkacct.breaksrt,"x(" + STRING(lname) + ")" ).
               WHEN "contract"   THEN PUT UNFORMATTED " " FILL(" ",10).
               WHEN "dubl_acct"  THEN PUT UNFORMATTED " " FILL(" ",25).
               WHEN "d_close" OR
               WHEN "d_open"     THEN PUT UNFORMATTED " " FILL(" ",9).
               WHEN "val1"    OR
               WHEN "val3"    OR
               WHEN "val4"    OR
               WHEN "val7"    OR
               WHEN "val9"    OR
               WHEN "val10"   OR
               WHEN "val11"   THEN DO:
                  idx = INT64 ( SUBSTR ( ENTRY ( j, dispcols ),4 ) ).
                  CASE idx:
                     WHEN 1 THEN
                     DO:
                        {genpotot.i 1}
                        RUN putnumber (vCurrentTotal[vPosBreaksrt]).
                     END.
                     WHEN 3 THEN
                     DO:
                        {genpotot.i 3}
                        RUN putnumber (vCurrentTotal[vPosBreaksrt]).
                     END.
                     WHEN 4 THEN
                     DO:
                        {genpotot.i 4}
                        RUN putnumber (vCurrentTotal[vPosBreaksrt]).
                     END.
                     WHEN 7 THEN
                     DO:
                        {genpotot.i 7}
                        RUN putnumber (vCurrentTotal[vPosBreaksrt]).
                     END.
                     WHEN 9 THEN
                     DO:
                        {genpotot.i 9}
                        RUN putnumber (vCurrentTotal[vPosBreaksrt]).
                     END.
                     WHEN 10 THEN
                     DO:
                        {genpotot.i 10}
                        RUN putnumber (vCurrentTotal[vPosBreaksrt]).
                     END.
                     WHEN 11 THEN
                     DO:
                        {genpotot.i 11}
                        RUN putnumber (vCurrentTotal[vPosBreaksrt]).
                     END.
                  END.
               END.
               WHEN "val2" OR
               WHEN "val5" OR
               WHEN "val6" OR
               WHEN "val8" THEN do:
                  idx = INT64 ( SUBSTR ( ENTRY ( j, dispcols ),4 ) ).
                  RUN putnumber ( ? ).
               END.
               WHEN "nloan" THEN
               DO:
                  PUT UNFORMATTED " " SPACE(35).
               END.
               WHEN "acct_rsrv"  THEN PUT UNFORMATTED FILL(" ",26).
               OTHERWISE DO:
                  FIND FIRST xattr-temp WHERE
                             xattr-temp.xattr-code = SUBSTR(ENTRY(j,dispcols),4,-1)
                  NO-ERROR.
                  PUT UNFORMATTED " " FILL(" ",xattr-temp.LENGTH).
               END.
            END CASE.
         END.
         PUT SKIP(2).
         IF    (LAST(ttSort.Sort1) AND vPosBreaksrt EQ 1)
            OR (LAST(ttSort.Sort2) AND vPosBreaksrt EQ 2)
            OR (LAST(ttSort.Sort3) AND vPosBreaksrt EQ 3)
            OR (LAST(ttSort.Sort4) AND vPosBreaksrt EQ 4)
            OR (LAST(ttSort.Sort5) AND vPosBreaksrt EQ 5) THEN 
         DO:
            PUT UNFORMATTED header3 SKIP.
            DO j = 1 TO NUM-ENTRIES(dispcols):
               CASE ENTRY(j,dispcols):
                  WHEN "acct"       THEN PUT UNFORMATTED FILL(" ",26).
                  WHEN "currency"   THEN PUT UNFORMATTED " " FILL(" ",3).
                  WHEN "since"      THEN PUT UNFORMATTED " " FILL(" ",10).
                  WHEN "side"       THEN PUT UNFORMATTED "    ".
                  WHEN "t_curr"     THEN PUT UNFORMATTED " " FILL(" ",12).
                  WHEN "custname"   THEN PUT UNFORMATTED " " FILL(" ",35).
                  WHEN "name"       THEN PUT UNFORMATTED " " STRING ( "Итого по отчету", "x(" + STRING(lname) + ")" ).
                  WHEN "contract"   THEN PUT UNFORMATTED " " FILL(" ",10).
                  WHEN "dubl_acct"  THEN PUT UNFORMATTED " " FILL(" ",25).
                  WHEN "d_close" OR
                  WHEN "d_open"     THEN PUT UNFORMATTED " " FILL(" ",9).
                  WHEN "val1"    OR
                  WHEN "val3"    OR
                  WHEN "val4"    OR
                  WHEN "val7"    OR
                  WHEN "val9"    OR
                  WHEN "val10"   OR
                  WHEN "val11"   THEN DO:
                     idx = INT64(substr(ENTRY(j,dispcols),4)).
                     CASE idx:
                        WHEN 1 THEN
                        DO:
                           {genpotot.i 1}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 3 THEN
                        DO:
                           {genpotot.i 3}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 4 THEN
                        DO:
                           {genpotot.i 4}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 7 THEN
                        DO:
                           {genpotot.i 7}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 9 THEN
                        DO:
                           {genpotot.i 9}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 10 THEN
                        DO:
                           {genpotot.i 10}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                        WHEN 11 THEN
                        DO:
                           {genpotot.i 11}
                           RUN putnumber (vCurrentTotal[6]).
                        END.
                     END.
                  END.
                  WHEN "val2" OR WHEN "val5" OR WHEN "val6" OR WHEN "val8" THEN do:
                     idx = INT64 ( SUBSTR ( ENTRY ( j, dispcols ),4 ) ).
                     RUN putnumber ( ? ).
                  END.
                  WHEN "nloan" THEN
                  DO:
                     PUT UNFORMATTED " " SPACE(35).
                  END.
                  WHEN "acct_rsrv"  THEN PUT UNFORMATTED FILL(" ",26).
                  OTHERWISE DO:
                     FIND FIRST xattr-temp WHERE
                                xattr-temp.xattr-code = SUBSTR(ENTRY(j,dispcols),4,-1)
                     NO-ERROR.
                     PUT UNFORMATTED " " FILL(" ",xattr-temp.LENGTH).
                  END.
               END CASE.
            END.
         END.
      END.
      END.
   END.
END PROCEDURE.

PROCEDURE putnumber.
   DEF INPUT PARAMETER bal AS DECIMAL NO-UNDO.

   IF idx < 3 OR idx > 6 THEN
      tmplb = FILL(" ", LENGTH(numformat) + 3).
   ELSE
      tmplb = FILL(" ", LENGTH(numformat)).
   IF bal EQ ? THEN OVERLAY(tmplb, INT64(LENGTH(tmplb) / 2)) = "X".
   ELSE IF NOT {acctlook.i} THEN OVERLAY(tmplb, 1) = "НЕТ ДОПУСКА".
   ELSE IF bal <> 0 OR (NOT zerospace AND bal = 0) THEN DO:
      IF idx < 3 OR idx > 6 THEN
         ASSIGN
            tmplb = STRING(bal, numformat + "   ") WHEN bal = 0
            tmplb = STRING(bal / units, numformat + " Дб") WHEN bal > 0
            tmplb = STRING(- bal / units, numformat + " Кр") WHEN bal < 0
         .
      ELSE
         tmplb = STRING(bal / units, numformat).
   END.
   PUT UNFORMATTED " " tmplb.
END PROCEDURE.

/* Сортировка данных запускать после PrepareHeader*/
PROCEDURE pSortData :
   DEFINE INPUT  PARAMETER iSort AS CHARACTER  NO-UNDO.

   DEFINE VARIABLE vI     AS INT64   NO-UNDO.
   DEFINE VARIABLE vValue AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vSort  AS CHARACTER NO-UNDO.
   
   IF     NUM-ENTRIES(iSort) LT 5 
      AND INDEX(iSort,"11 + валюта") EQ 0 THEN
      iSort = iSort + ",11 + валюта".
   FOR EACH wrkacct NO-LOCK,
      FIRST acct WHERE RECID(acct) = wrkacct.id NO-LOCK:

      CREATE ttSort.
      ASSIGN
         ttSort.Rwd = ROWID(wrkacct)
         vI         = 0
      .
      IF TRIM(iSort,",") NE "" THEN
      DO vI = 1 TO MINIMUM(5,NUM-ENTRIES(iSort)):
         CASE TRIM(ENTRY(vI,iSort)):
            WHEN "Балансовый счет" THEN
               ASSIGN
                  vValue = wrkacct.breaksrt
                  vSort  = "балансовому счету "
               .
            WHEN "11 + валюта" THEN
               ASSIGN
                  vValue = wrkacct.srtfield
                  vSort  = ""
               .
            WHEN "Счет" /*"acct"*/ THEN
               ASSIGN
                  vValue = wrkacct.breaksrt
                  vSort  = "счету "
               .
            WHEN "Валюта" /*"currency"*/ THEN
               ASSIGN
                  vValue = STRING(wrkacct.currency,"XXX")
                  vSort  = "валюте"
               .
            WHEN "А/П" /*"side"*/ THEN
               ASSIGN
                  vValue = STRING(acct.side," XX")
                  vSort  = "реквизиту"
               .
            WHEN "Дата открытия" /*"d_open"*/ THEN
               ASSIGN
                  vValue = STRING(acct.open-date,"99/99/9999")
                  vSort  = "дате открытия  "
               .
            WHEN "Дата закрытия" /*"d_close"*/ THEN
               ASSIGN 
                  vValue = IF acct.close-date = ? 
                           THEN "          "
                           ELSE STRING(acct.close-date,"99/99/9999")
                  vSort  = "дате закрытия "
               .
            WHEN "Парный счет" /*"dubl_acct"*/ THEN
               ASSIGN
                  vValue = STRING(acct.contr-acct,"X(25)")
                  vSort  = "парному счету"
               .
            WHEN "Тип курса" /*"t_curr"*/ THEN
               ASSIGN
                  vValue = STRING(acct.rate-type,"X(12)")
                  vSort  = "типу курса"
               .
            WHEN "ДПД" /*"since"*/ THEN
               ASSIGN
                  vSort  = "ДПД"
                  vValue = IF wrkacct.since = ? 
                           THEN "          "
                           ELSE STRING(wrkacct.since,"99/99/9999")
               .
            WHEN "Наименование клиента" /*"name"*/ THEN
            DO:
               vValue = STRING(wrkacct.cliname[1],"X(35)").
               IF vValue EQ "" THEN vValue = " -------".
               vSort  = "наименованию ".
            END.
            WHEN "Наименование" /*"name"*/ THEN
            DO:
               vValue = STRING(wrkacct.acctname,"X(35)").
               IF vValue EQ "" THEN vValue = " -------".
               vSort  = "наименованию ".
            END.
            WHEN "Назначение" /*"contract"*/ THEN
                ASSIGN
                  vValue = STRING(acct.contract,"x(10)")
                  vSort  = "назначению "
               .
            WHEN "Исх. остаток" /*"val1"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[1],"-9999999999999999999999999.9999").
            WHEN "Исх.остаток в вал." /*"val2"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[2],"-9999999999999999999999999.9999").
            WHEN "Дебет" /*"val3"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[3],"-9999999999999999999999999.9999").
            WHEN "Кредит" /*"val4"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[4],"-9999999999999999999999999.9999").
            WHEN "Дебет в вал." /*"val5"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[5],"-9999999999999999999999999.9999").
            WHEN "Кредит в вал." /*"val6"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[6],"-9999999999999999999999999.9999").
            WHEN "Вход. остаток" /*"val7"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[7],"-9999999999999999999999999.9999").
            WHEN "Вход.остаток в вал." /*"val8"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[8],"-9999999999999999999999999.9999").
            WHEN "Номер договора" /*"nloan"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.cont-code,"x(35)").
            WHEN "Дебет количества" /*"val9"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[9],"-9999999999999999999999999.9999").
            WHEN "Кредит количества" /*"val10"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[10],"-9999999999999999999999999.9999").
            WHEN "Количество" /*"val11"*/ THEN
               ASSIGN
               vSort  = "реквизиту"
               vValue = STRING(wrkacct.val[11],"-9999999999999999999999999.9999").
            WHEN "Счет резерва" THEN
               ASSIGN
                  vSort  = "счету резерва"
                  vValue = wrkacct.acct-rsrv.   
             /* Значит, допреквизит */
            OTHERWISE DO:
               vSort = " реквизиту".
               FIND FIRST xattr-temp WHERE
                          xattr-temp.xattr-code = SUBSTR(ENTRY(vI,iSort),4)
               NO-LOCK NO-ERROR.

               IF IsTemporal("acct", xattr-temp.xattr-code) THEN
                  vValue = GetTempXAttrValue("acct",
                                             acct.acct + "," + acct.currency,
                                             xattr-temp.xattr-code).
               ELSE
                  vValue = GetXAttrValue("acct",
                                         acct.acct + "," + acct.currency,
                                         xattr-temp.xattr-code).
               IF vValue = "" THEN
                  vValue = " ".
               ELSE DO:
                  CASE xattr-temp.data-type:
                     WHEN "date"    THEN vValue = STRING(INT64(DATE(vValue)),"99999999999999999").
                     WHEN "integer" OR
                     WHEN "decimal" THEN vValue = STRING(DECIMAL(vValue),"-9999999999999999999999999.9999").
                  END CASE.
               END.
            END.
         END CASE.

         CASE vI:
            WHEN 1 THEN 
               ASSIGN
                   ttSort.Sort1    = vValue
                   ttSort.SortName = vSort
               .
            WHEN 2 THEN ttSort.Sort2 = vValue.
            WHEN 3 THEN ttSort.Sort3 = vValue.
            WHEN 4 THEN ttSort.Sort4 = vValue.
            WHEN 5 THEN ttSort.Sort5 = vValue.
         END CASE.
      END.
      RELEASE ttSort.
   END. /* FOR EACH tmprecid */

END PROCEDURE.

