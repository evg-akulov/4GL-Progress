/*
                Банковская интегрированная система БИСквит
    Copyright:  (C) 1992-2000 ТОО "Банковские информационные системы"
     Filename:  impbanks.p
      Comment:  Загрузка справочника банков
   Parameters:  fi - тип файла
                1  - bnkseek.dbf
                2  - bnk-sng.dbf
                3  - ?
                4  - bnkdel.dbf
         Uses:  impbanks.imp imp-corr.i imprem.i
      Used BY:
      Created:
     Modified:  22/02/2000 Mike - протокол импорта для банков-клиентов
                                - перепривязка счетов, если банк меняет БИК
     Modified:  30/10/2003 NIK  Положение 225-П
     Modified:  09/11/2005 kraw (0052636) не пытаться "отозвать" несуществующий банк
     Modified:  15/07/2010 kraw (0129464) включим триггер на запись при подгрузке bnkseek

Чтобы сделать такой импорт, надо искорёжить impbanks.p, разумеется под новым именем:
- другие input parameters, а конкретно
Код:
def input param in-op-date as date no-undo.
def input param in-rec-kind as recid no-undo.
- убрать любые упоминания о фреймах info и frAll
- поставить разумные умолчания переменным fr и mUpdAll
- вместо {getfile.i} установить переменной source значение и условием запуска поставить наличие файла
- убрать вопрос "Банк ... исправляли вручную" и выбрать умолчание
- убрать все остальные message, {message} и justamin

Сейчас такую переделанную наблюдаю в режиме тестирования на живом справочнике. Вроде, пока работает.

*/
{globals.i}
{intrface.get xclass}
{intrface.get date}     /* Инструменты для работы с датами. */
{debug.equ}

DEFINE INPUT PARAMETER fi AS INT64 NO-UNDO.

&SCOP DEL-BANK  "ОТЗВ"

DEF VAR gos1         AS CHAR INIT "33,34,35,36,37,38,39,43,44,48,49,53,54,55,56"                                                                                                    NO-UNDO.
DEF VAR gos2         AS CHAR INIT "УКРАИНА,БЕЛОРУССИЯ,КАЗАХСТАН,УЗБЕКИСТАН,ТАДЖИКИСТАН,ТУРКМЕНИЯ,КИРГИЗИЯ,МОЛДАВИЯ,АРМЕНИЯ,АЗЕРБАЙДЖАН,ГРУЗИЯ,ЛАТВИЯ,ЛИТВА,ЭСТОНИЯ,ПРИДНЕСТРОВЬЕ"   NO-UNDO.
DEF VAR pzn1         AS CHAR INIT "00,10,11,20,21,22,23,24,25,26,30,31,32,33,34,35,36,37,40,50,70,71,72,73,74,90,91,98,99,,**"                                                         NO-UNDO.
DEF VAR pzn2         AS CHAR INIT "ГРКЦ,РКЦ,ПРС,Б,КБ,СБ,АКБ,ЧКБ,КОПБ,АПБ,ФБ,ФКБ,ФСБ,ФАКБ,ФЧКБ,ФКОБ,Отд.,ТУСБ,ПУ,ЦХ,НКО,КЛ,ОРЦБ,НДКО,ФНКО,ЛИКВ,КУПР,ИСКЛ,ОТЗВ,,"                         NO-UNDO.
DEF VAR punkt1       AS CHAR INIT "1,2,3,4,5,6,7"                                                                                                                                   NO-UNDO.
DEF VAR punkt2       AS CHAR INIT "Г,П,С,ПГТ,СТ-ЦА,АУЛ,РП"                                                                                                                          NO-UNDO.
DEF VAR struct-bis   AS CHAR INIT "Pzn,Ind,Tnp,Nnp,Adr,Namep,Namen,Newnum,Permfo,Srok,Telef,Dt-Izm,Regn,Rkc,Ksnp,uer,rgn,real,Vkey,DATE-CH,VKEYDEL,OKPO"                            NO-UNDO.
                               /*   1   2   3   4   5    6     7      8      9    10    11    12    13   14  15  16  17  18    19   20       21     22*/

/*VKEY,REAL,PZN,UER,RGN,IND,TNP,NNP,ADR,RKC,NAMEP,NAMEN,NEWNUM,NEWKS,PERMFO,SROK,AT1,AT2,TELEF,REGN,OKPO,DT_IZM,P,CKS,KSNP,DATE_IN,UERKO*/


DEF VAR source       AS CHAR FORMAT "x(60)"                 NO-UNDO.
DEF VAR struct-dbf   AS CHAR                                NO-UNDO.
DEF VAR st           AS CHAR FORMAT "x(20)" LABEL "Статус"  NO-UNDO.
DEF VAR s            AS CHAR EXTENT 40                      NO-UNDO.
DEF VAR n-mfo        AS CHAR FORMAT "x(9)"                  NO-UNDO.
DEF VAR v-key        AS CHAR                                NO-UNDO.
DEF VAR date-ch      AS CHAR                                NO-UNDO.
DEF VAR v-keydel     AS CHAR                                NO-UNDO.
DEF VAR vOKPO        AS CHAR                                NO-UNDO.
DEF VAR num          AS INT64                               NO-UNDO.
DEF VAR uer          AS CHAR                                NO-UNDO.
DEF VAR uer-ko       AS CHAR                                NO-UNDO.
DEF VAR region       AS CHAR                                NO-UNDO.
DEF VAR real         AS CHAR                                NO-UNDO.
DEF VAR num-max      AS INT64                               NO-UNDO.
DEF VAR j            AS INT64                               NO-UNDO.
DEF VAR i            AS INT64 EXTENT 40                     NO-UNDO.
DEF VAR counter      AS INT64                               NO-UNDO.

DEF VAR mUpdAll      AS LOGICAL  INIT NO
                        LABEL    "Изменять банки"
                        FORMAT   "Все"              NO-UNDO.

DEF TEMP-TABLE rem NO-UNDO LIKE banks.

DEF BUFFER     buf-banks   FOR banks.
DEF BUFFER     xbanks      FOR banks.
DEF BUFFER     xsigns      FOR signs.
DEF BUFFER     dbanks      FOR banks.
DEF BUFFER     dbanks-code FOR banks-code.

DEF STREAM err.
DEF STREAM impstr.
def stream log_file_1.
def stream log_file_2.

DEF TEMP-TABLE banks-var   NO-UNDO
         FIELD bic         AS CHAR
         FIELD name        AS CHAR
         FIELD attr-bnk    AS CHAR
         FIELD old-value   AS CHAR
         FIELD new-value   AS CHAR
.

ON DELETE OF banks override DO: END.


/*============================================================================*/

CASE fi:
   WHEN 1 THEN
INP:
   DO ON ERROR UNDO INP, RETURN:
      source = "/home2/bis/quit41d/imp-exp/BNKSEEK.DBF".
      mUpdAll=Yes. 
   END.
   WHEN 2 THEN source = "bnk-sng.dbf".
   WHEN 3 THEN source = "uch.dbf".
   WHEN 4 THEN source = "/home2/bis/quit41d/imp-exp/BNKDEL.DBF".
   OTHERWISE   source = "".
END CASE.

RUN XAttrAll IN h_xclass ("banks", OUTPUT TABLE xattrid).

/*MESSAGE "Конвертация файла" source "в текстовый формат.".*/

/*{justamin} выводит снизу Минуточку*/
IF SEARCH("./impbanks.d") EQ ? THEN DO:
   IF OPSYS EQ "unix" THEN DO:
      unix silent dbf 1 1 VALUE(source) /dev/null > "impbanks.d".
      unix silent dbf 2 1 VALUE(source) /dev/null > "./impbanks.df".
   END.
   ELSE DO:
      dos silent dbf 1 1 VALUE(source) nul > "impbanks.d".
      dos silent dbf 2 1 VALUE(source) nul > "impbanks.df".
   END.
END.

                                         /* Определение числа записей в файле */
INPUT STREAM impstr FROM  VALUE(source).
st = "".
DO j = 1 TO 4 :
   READKEY STREAM impstr PAUSE 0.
END.
DO j = 1 TO 2 :
   READKEY STREAM impstr PAUSE 0.
   st = st + chr(LASTKEY).
END.

num-max = 256 * (IF  asc(SUBSTRING(st,2,1)) > 0 THEN
                     asc(SUBSTRING(st,2,1)) ELSE 0) +
                     asc(SUBSTRING(st,1,1)).
INPUT STREAM impstr CLOSE.

IF SEARCH("./impbanks.df") NE ? THEN DO:
   INPUT  STREAM impstr FROM "./impbanks.df".
   IMPORT STREAM impstr  ^.

   REPEAT:
      IMPORT STREAM impstr s.
      {additem.i struct-dbf s[2]}
   END.

   s[1] = "".
   IF fi EQ 1 OR
      fi EQ 3 OR
      fi EQ 4 THEN
   DO j = 1 TO NUM-ENTRIES(struct-bis):
      i[j] = LOOKUP(ENTRY(j,struct-bis),struct-dbf).
      IF i[j] EQ 0 THEN DO:
         {additem.i s[1] ENTRY(j,struct-bis) }
      END.
   END.
   IF s[1] NE "" AND fi = 1 THEN DO:
/*      {message &text="|Не та версия справочника банков !|Нет полeй: ||"" + s[1] + "" !"}*/
      INPUT STREAM impstr CLOSE.
      RETURN.
   END.
   INPUT STREAM impstr CLOSE.
END.

PAUSE(0).
/*MESSAGE "".*/
PAUSE(0).

/*----------------------------------------------------------------------------*/

INPUT STREAM impstr FROM impbanks.d.

CREATE rem.

main:
REPEAT:
   {on-esc LEAVE main}

   IMPORT STREAM impstr s.

   num = num + 1.

   {imprem.i}

   CASE fi:
      WHEN 1 OR
      WHEN 2 OR
      WHEN 3 THEN FIND banks-code WHERE banks-code.bank-code-type EQ "МФО-9"
                                    AND banks-code.bank-code      EQ n-mfo
                                        NO-LOCK NO-ERROR.
      WHEN 4 THEN FIND banks-code WHERE banks-code.bank-code-type EQ "VKEY"
                                    AND banks-code.bank-code      EQ v-key
                                        NO-LOCK NO-ERROR.
      OTHERWISE   RELEASE banks-code.
   END.

   IF NOT AVAILABLE(banks-code) AND fi GE 4 THEN NEXT main.

   IF AVAILABLE(banks-code) THEN
      FIND banks OF banks-code EXCLUSIVE-LOCK.

   IF NOT AVAILABLE(banks-code) THEN DO:
      st = "Добавляется".
      CREATE banks.
      ASSIGN
         banks.country-id     = rem.country-id
         banks.country        = rem.country
         banks.flag-rkc       = rem.flag-rkc
         banks.law-address    = rem.law-address
         banks.mail-address   = rem.mail-address
         banks.name           = rem.name
         banks.short-name     = rem.short-name
         banks.bank-type      = rem.bank-type
         banks.time-doc       = rem.time-doc
         banks.town           = rem.town
         banks.town-type      = rem.town-type
         banks.last-date      = rem.last-date
      .
   END.
   ELSE DO:
      IF banks.modified THEN DO:
/*         {message &text="|Банк "" + n-mfo + "" "" + rem.short-name + "" исправляли вручную.|Вносить изменения?"
                  &alert-box=question
                  &buttons=yes-no
         }
         IF pick-value NE "yes" THEN NEXT main.*/
      END.
      ELSE
         IF NOT mUpdAll                      AND
            banks.last-date GT rem.last-date THEN
            NEXT main.
      st = "Изменяется".
   END.


   CASE fi:
      WHEN 1 OR
      WHEN 3 THEN DO:
         IF banks.client THEN DO:
            ON WRITE OF banks REVERT.
            RUN banks-cmp.
         END.
         ELSE DO:

            IF fi NE 1 THEN
               ON WRITE OF banks override DO: END.
         END.

         ASSIGN
            banks.country-id     = rem.country-id
            banks.country        = rem.country
            banks.flag-rkc       = rem.flag-rkc
            banks.law-address    = rem.law-address
            banks.mail-address   = rem.mail-address
            banks.name           = rem.name
            banks.short-name     = rem.short-name
            banks.time-doc       = rem.time-doc
            banks.town           = rem.town
            banks.town-type      = rem.town-type
            banks.last-date      = rem.last-date
            banks.modified       = NO
         .

         banks.bank-type = IF GetCodeMisc("bnk-real",real,1) EQ "Да"
                              THEN real
                              ELSE rem.bank-type.

         {impbanks.imp banks "МФО-9" n-mfo      "/*" }
         {impbanks.imp banks "VKey"  v-key      "/*" }

         &IF DEFINED(IS-DEBUG) &THEN
         RUN dbgprint.p ("Impbanks"," mfo-9:"    + n-mfo +
                                    " v-key:"    + v-key +
                                    " v-keydel:" + v-keydel).
         &ENDIF

         IF {assigned v-keydel} THEN DO:         /* Преемник                  */
            {getbank.i dbanks v-keydel 'VKey'}

            IF AVAILABLE dbanks THEN DO:
               FIND FIRST dbanks-code OF dbanks WHERE
                          dbanks-code.bank-code-type EQ "МФО-9"
                          NO-LOCK NO-ERROR.
               v-keydel = IF AVAILABLE(dbanks-code) AND
                             dbanks-code.bank-code NE n-mfo
                             THEN dbanks-code.bank-code
                             ELSE "".
            END.
            ELSE v-keydel = "".
         END.
         date-ch = STRING(Str2DateEngl(date-ch),"99/99/9999").
         UpdateSigns("banks", string(banks.bank-id), "uer",          uer,      NO).
         UpdateSigns("banks", string(banks.bank-id), "region",       region,   YES).
         UpdateSigns("banks", string(banks.bank-id), "ДатаКонтроля", date-ch,  YES).
         UpdateSigns("banks", string(banks.bank-id), "Преемник",     v-keydel, YES).
         UpdateSigns("banks", string(banks.bank-id), "OKPO",         vOKPO,    NO).

/*         IF {assigned v-keydel} AND  DATE(date-ch) GE TODAY THEN 
            RUN Fill-SysMes ("","ImpBanks01","1","%s=" + n-mfo    +
                                                 "%s=" + v-keydel +
                                                 "%s=" + date-ch).*/

         IF real NE "ИРКЦ" THEN
            UpdateSigns("banks", string(banks.bank-id), "real", real,   ?).


         IF banks.flag-rkc THEN DO:
            {impbanks.imp banks "МФО-6" s[i[9]] "/*" }
         END.
         {impbanks.imp banks "REGN" s[i[13]] "/*" }

         {imp-corr.i "МФО-9" s[i[14]] s[i[15]]}
      END.

      WHEN 2 THEN DO:
         ASSIGN
            banks.country-id     = rem.country-id
            banks.country        = rem.country
            banks.flag-rkc       = rem.flag-rkc
            banks.law-address    = rem.law-address
            banks.mail-address   = rem.mail-address
            banks.name           = rem.name
            banks.short-name     = rem.short-name
            banks.bank-type      = rem.bank-type
            banks.time-doc       = rem.time-doc
            banks.town           = rem.town
            banks.town-type      = rem.town-type
            banks.last-date      = rem.last-date
            banks.modified       = NO
         .
         {impbanks.imp banks "МФО-9" n-mfo "/*" }
         IF banks.flag-rkc THEN DO:
            {impbanks.imp banks "МФО-6" s[5] "/*" }
         END.
         {imp-corr.i "МФО-9" s[3] s[10]}
      END.

      WHEN 4 THEN DO:
         UpdateSigns("banks",
                     string(banks.bank-id),
                     "real",
                     {&DEL-BANK},
                     ?).
         ASSIGN
            banks.last-date  = rem.last-date
            banks.modified   = NO
         .

         IF GetCodeMisc("bnk-real",{&DEL-BANK},1) EQ "Да" THEN
            banks.bank-type = {&DEL-BANK}.

         RUN banks-cmp.
      END.
   END.
END.                                             /* main: REPEAT:             */

INPUT STREAM impstr CLOSE.

IF fi LT 5 THEN DO:
   num = 0.
   INPUT STREAM impstr FROM impbanks.d.
main2:
   REPEAT:
      {on-esc LEAVE main2}

      num = num + 1.
      IMPORT STREAM impstr s.
      {imprem.i}


      FIND FIRST banks-code WHERE
                 banks-code.bank-code-type EQ "МФО-9"
             AND banks-code.bank-code      EQ n-mfo
                 NO-LOCK NO-ERROR.

      IF NOT AVAILABLE(banks-code) THEN
         NEXT main2.

      FIND FIRST banks OF banks-code NO-LOCK.

      IF banks.last-date > rem.last-date THEN NEXT main2.
      st = "Изменяется".

      IF banks.flag-rkc THEN DO:
         {impbanks.imp banks "МФО-6" "(IF fi = 1 OR fi = 3 THEN s[i[9]] ELSE IF fi = 4 THEN s[6] ELSE IF fi = 5 THEN s[2] ELSE s[5])" "/*" }
      END.
   END.
   INPUT STREAM impstr CLOSE.
END.

IF OPSYS EQ "unix" THEN unix silent rm  impbanks.d*.
                   ELSE dos  silent del impbanks.d*.

/*{message "|Загружено "" + string(num - 1) + "" банков."}*/

FIND FIRST  banks-var NO-ERROR.
IF AVAIL banks-var THEN DO:
/*  {setdest.i &filename="1.log"}
   PUT UNFORMATTED "ПРОТОКОЛ ИЗМЕНЕНИЙ БАНКОВ-КЛИЕНТОВ. "
                   string(today,"99/99/9999") " " string(time,"hh:mm:ss")
                   SKIP(1).

   FOR EACH banks-var BREAK BY banks-var.bic:
      IF FIRST-OF(banks-var.bic) THEN
         PUT UNFORMATTED SKIP banks-var.bic "  " banks-var.name SKIP(1).

      PUT UNFORMATTED " " string(banks-var.attr-bnk,"x(13)") + ": "
                          string(banks-var.old-value,"x(33)") + " | " +
                          string(banks-var.new-value,"x(33)") SKIP.
   END.
   {preview.i}*/
END.

num = num - 1.

IF fi = 1 THEN DO:
	output stream log_file_1 to value("/var/log/log_bnkseek.log").   
        put stream log_file_1 "ЗАГРУЖЕНО БАНКОВ-КЛИЕНТОВ " num skip(1).
        output stream log_file_1 close.
END.

IF fi = 4 THEN DO:
	output stream log_file_2 to value("/var/log/log_bnkdel.log").   
	put stream log_file_2 "УДАЛЕНО БАНКОВ-КЛИЕНТОВ " num skip(1).
	output stream log_file_2 close.
END.

IF fi = 1 THEN RUN 66impbanks.p (4).

{intrface.del}          /* Выгрузка инструментария. */ 


/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE banks-cmp.
   {impbanks.cmp banks.country        rem.country       "Страна"}
   {impbanks.cmp banks.law-address    rem.law-address   "Адрес"}
   {impbanks.cmp banks.mail-address   rem.mail-address  "П/Адрес"}
   {impbanks.cmp banks.name           rem.name          "Наименование"}
   {impbanks.cmp banks.short-name     rem.short-name    "Краткое наим."}
   {impbanks.cmp banks.bank-type      rem.bank-type     "Тип"}
   {impbanks.cmp banks.town           rem.town          "Город"}
   {impbanks.cmp banks.town-type      rem.town-type     "Тип нас.пункта"}
END PROCEDURE.
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*----------------------------------------------------------------------------*/
PROCEDURE get_init_val:
   DEF INPUT  PARAM in-class AS CHAR         NO-UNDO.
   DEF INPUT  PARAM in-code  AS CHAR         NO-UNDO.
   DEF OUTPUT PARAM out-val  AS CHAR INIT ?  NO-UNDO.

   FIND FIRST xattrid WHERE  xattrid.xattr-code = in-code NO-LOCK NO-ERROR.

   FIND FIRST xattr WHERE
              xattr.class-code = xattrid.class-code
          AND xattr.xattr-code = xattrid.xattr-code NO-LOCK NO-ERROR.

   IF AVAIL xattr THEN out-val =  xattr.INITIAL.
END PROCEDURE.
/******************************************************************************/
