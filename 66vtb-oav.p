/*             Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2002 ТОО "Банковские информационные системы"
     Filename: vtb-oav.p
      Comment: Извещение об открытии счетов 
         Uses:
      Created: alvel 21.10.2002
     Modified:

qq  убрать  за дату
qq добавила ОАО Россельхозбанк  после № счета в..
qq в метасхеме на branch.ИзвНачОО расширила формат с 65 до 70, т.к. не входит значение 
   sirotrin - изменен алгоритм формирования КПП для ГО
*/

{globals.i}
{bank-id.i}
{intrface.get cust}

{tmprecid.def}

&SCOP OFFSET_VAL 480 /* сдвиг от верхнего края страницы */

DEFINE STREAM fil.
DEFINE STREAM mfil.
DEFINE STREAM macro-file.

DEFINE BUFFER xprinter FOR PRINTER.

DEFINE VARIABLE account-date   AS DATE                     NO-UNDO. /* дата открытия счета */
DEFINE VARIABLE account        AS CHARACTER FORMAT 'x(20)' NO-UNDO. /* номер счета */
DEFINE VARIABLE name-client    as CHARACTER FORMAT 'x(71)' NO-UNDO. /* клиент - владелец счета */
DEFINE VARIABLE current-date   AS DATE                     NO-UNDO. /* текущая календарная дата */
DEFINE VARIABLE account-target AS CHARACTER                NO-UNDO. /* цель счета (доп.реквизит) */
DEFINE VARIABLE rkc-name       AS CHARACTER                NO-UNDO. /* название РКЦ */
DEFINE VARIABLE acct-place     AS CHARACTER                NO-UNDO. /* место открытия счета */
DEFINE VARIABLE chief-oo       AS CHARACTER                NO-UNDO. /* начальник операционного отдела */
DEFINE VARIABLE addr           AS CHARACTER                NO-UNDO. /* юр. адрес */
DEFINE VARIABLE mi             AS INT64                    NO-UNDO.
DEFINE VARIABLE buf-str        AS CHARACTER INITIAL ""     NO-UNDO.
DEFINE VARIABLE i              AS INT64                    NO-UNDO.
DEFINE VARIABLE init-str       AS CHARACTER                NO-UNDO.
DEFINE VARIABLE reset-str      AS CHARACTER                NO-UNDO.
DEFINE VARIABLE L-Sh           AS INT64                    NO-UNDO.
DEFINE VARIABLE s-leng         AS INT64                    NO-UNDO.
DEFINE VARIABLE Sec            AS INT64                    NO-UNDO.
DEFINE VARIABLE macro-prn      AS LOGICAL INIT FALSE       NO-UNDO.
DEFINE VARIABLE mName          AS CHARACTER EXTENT 2       NO-UNDO.
DEFINE VARIABLE mINN           AS CHARACTER                NO-UNDO.
DEFINE VARIABLE mKPP           AS CHARACTER                NO-UNDO.
DEFINE VARIABLE mOGRN          AS CHARACTER                NO-UNDO.

ASSIGN
   L-Sh   = 180  /* смещ-у от лев.края */
   s-leng = 20   /* ширина символа */
   Sec    = 1400 /* начало (смещение) второго экземпляра */
.

{setdest.i &stream="stream fil " &cols = 68}
{get_set2.i "Принтер" "PCL" "w/o chek"}

IF    ((NOT AVAIL(setting) OR (AVAIL(setting) AND TRIM(setting.val) = "")) 
   AND usr-printer BEGINS "+")
   OR (AVAIL(setting) AND CAN-DO(setting.val, usr-printer)) THEN DO:

   {0401060.prg}
 
   OUTPUT STREAM macro-file TO "_macro.tmp".
 
   ASSIGN
      init-str  = ""
      reset-str = ""
   .
 
   FIND FIRST xprinter WHERE xprinter.printer = usr-printer 
                         AND xprinter.page-cols <= 80 NO-LOCK NO-ERROR.
 
   IF AVAIL(xprinter) THEN DO:
      IF xprinter.init-string <> ? AND TRIM(xprinter.init-string) <> "" THEN
        init-str = vf_xcode(xprinter.init-string).
      IF xprinter.reset-string <> ? AND TRIM(xprinter.reset-string) <> "" THEN
        reset-str = vf_xcode(xprinter.reset-string).                          
   END.
 
   IF init-str <> "" THEN
      PUT STREAM macro-file UNFORMATTED init-str.
   macro-prn = TRUE.
END.

FOR EACH tmprecid,
   FIRST acct WHERE RECID(acct) = tmprecid.id NO-LOCK:
   
   /* извещения печатаются для частных и юридических лиц */
   IF NOT (acct.cust-cat EQ "ч" OR acct.cust-cat EQ "ю") THEN 
      NEXT.

   RUN GetCustName IN h_base (INPUT acct.cust-cat,
                              INPUT acct.cust-id,
                              INPUT acct.acct,
                              OUTPUT mName[1],
                              OUTPUT mName[2],
                              INPUT-OUTPUT mInn).

   ASSIGN
      name-client    = mName[1] + " " + mName[2]
      account-date   = acct.open-date
      account        = acct.acct
      current-date   = TODAY
      account-target = GetXattrValue("acct",STRING(acct.acct + "," + acct.currency),"ЦельСч")
   .
   
   FIND FIRST branch WHERE branch.branch-id EQ dept.branch NO-LOCK NO-ERROR.
   IF NOT AVAIL branch THEN RETURN.  

   {get_set.i "Адрес_юр"}
   ASSIGN
      acct-place = GetXattrValue("branch",dept.branch,"ИзвГдеОткр")
      addr       = setting.val
      rkc-name   = {banknm.lf banks}
      chief-oo   = GetXattrValue("branch",dept.branch,"ИзвНачОО")
      mOGRN      = GetXattrValue("branch",dept.branch,"ОГРН")
      mOGRN      = IF {assigned mOGRN}
                   THEN " ОГРН " + mOGRN
                   ELSE ""
   .
   /* SIR */
   
   IF shFilial = "0000" THEN DO:
      mKpp = FGetSetting("ГНИ","КППГНИ",?).
   END.
   IF NOT {assigned mKPP}  THEN DO:
      mKPP       = GetXattrValue("branch",dept.branch,"КПП").
   END.
   mKPP       = IF {assigned mKPP} THEN "/КПП " + mKPP ELSE "".

   DO mi=1 TO 2:
      PUT STREAM fil SKIP (5).
      PUT STREAM fil UNFORMATTED TRIM(STRING(name-client,'x(81)')) SKIP (1).
      PUT STREAM fil SKIP (5).
      PUT STREAM fil UNFORMATTED
      "Сообщаем, что " + STRING(account-date,'99/99/9999') + " Вам открыт " + account-target SKIP.
      PUT STREAM fil UNFORMATTED "счет № " + string(account,'x(20)') + " по договору " SKIP(1).
      PUT STREAM fil UNFORMATTED "      " + STRING(acct-place) + ":" SKIP .
      PUT STREAM fil UNFORMATTED "      " + STRING(addr) SKIP.       /* юридический адрес     */
      PUT STREAM fil UNFORMATTED "      БИК " + STRING(bank-mfo-9).   /* БИК банка             */
      PUT STREAM fil UNFORMATTED ", ИНН " + STRING(bank-inn) + mKPP + mOGRN SKIP.    /* ИНН банка             */
      PUT STREAM fil UNFORMATTED "      к/с " + STRING(bank-acct) SKIP.   /* корр. счет       */ 
      PUT STREAM fil UNFORMATTED "      в " + STRING(rkc-name) + "." SKIP (1).
      put STREAM fil unformatted "Дополнительно сообщаем, что в соответствии с Налоговым Кодексом РФ" SKIP.
      put STREAM fil unformatted "Федеральным законом 212-ФЗ, Вам необходимо в  7-и  дневный срок" SKIP.
      put STREAM fil unformatted "уведомить налоговые органы, Пенсионный фонд, Фонд Соц.Страхования об" SKIP.
      put STREAM fil unformatted "открытии вышеуказанного счета." SKIP (1).
      put STREAM fil unformatted "за " + STRING(current-date,'99/99/9999') SKIP (1).   
      PUT STREAM fil UNFORMATTED STRING(chief-oo) SKIP (1).
      put STREAM fil UNFORMATTED
      "--------------------------------------------------------------------" SKIP (1).
   END.
   PAGE STREAM fil.

   IF macro-prn THEN DO:
      buf-str = "".
      DO i=1 TO 2:
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 50 + {&OFFSET_VAL},  TRIM(string(name-client,'x(81)')),INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 350 + {&OFFSET_VAL}, "Сообщаем, что " + STRING(account-date,'99/99/9999') + " Вам открыт " + account-target, INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 400 + {&OFFSET_VAL}, "счет № " + string(account,'x(20)') + " в ОАО Россельхозбанк " + STRING(acct-place) + ":", INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 550 + {&OFFSET_VAL}, "      " + string(addr) , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 600 + {&OFFSET_VAL}, "      БИК " + string(bank-mfo-9) + ", ИНН " + string(bank-inn) + mKPP + mOGRN, INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 650 + {&OFFSET_VAL}, "      к/с " + string(bank-acct) , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 700 + {&OFFSET_VAL}, "      в " + STRING(rkc-name) + "." , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 800 + {&OFFSET_VAL}, "Дополнительно сообщаем, что в соответствии с Налоговым Кодексом РФ" , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 850 + {&OFFSET_VAL}, "Вам необходимо в  7-и  дневный срок уведомить налоговые органы об" , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 900 + {&OFFSET_VAL}, "открытии вышеуказанного счета." , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 1000 + {&OFFSET_VAL}, "за " + string(current-date,'99/99/9999') , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 1100 + {&OFFSET_VAL}, STRING(chief-oo) , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 1200 + {&OFFSET_VAL}, "--------------------------------------------------------------------" , INPUT-OUTPUT buf-str).
      END.
      PUT STREAM macro-file UNFORMATTED buf-str .
      PUT STREAM macro-file UNFORMATTED CHR(27) + CHR(38) + "l0H" .
   END.
END.

IF macro-prn THEN DO:  
  /* PUT stream macro-file unformatted chr(12). */
   IF reset-str <> "" THEN
      PUT STREAM macro-file UNFORMATTED reset-str.
   OUTPUT STREAM macro-file CLOSE.
END.

{preview.i &stream="stream fil "}

PROCEDURE PUT_PCL_STR:
   DEFINE INPUT         PARAMETER in-X      AS INT64       NO-UNDO.
   DEFINE INPUT         PARAMETER in-Y      AS INT64       NO-UNDO.
   DEFINE INPUT         PARAMETER in-str    AS CHARACTER   NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER out-str   AS CHARACTER   NO-UNDO.
   in-str = IF in-str EQ ? THEN "" ELSE in-str.
   out-str = out-str +
             CHR(27) + "*p"  + STRING(in-X) + "X" +
             CHR(27) + "*p"  + STRING(in-Y) + "Y" + " " +
             in-str.
END PROCEDURE.

{intrface.del}
