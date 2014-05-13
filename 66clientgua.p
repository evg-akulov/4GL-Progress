/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2010 ЗАО "Банковские информационные системы"
     Filename: CLIENTGUA.P
      Comment: 0131666 оперативный отчет по договорам обеспечения выделенных клиентов
   Parameters:"Ю","Ч" 
         Uses:
      Used by:
      Created: 19.10.2010 12:33 feok    
     Modified: 19.10.2010 12:33 feok    
*/

{globals.i}
{intrface.get i254} 
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{tmprecid.def}          /* Таблица с выбранными записями. */

DEF INPUT PARAM iParam AS CHAR NO-UNDO.

DEF VAR vTmpStr      AS CHAR   NO-UNDO.
DEF VAR mSurr        AS CHAR   NO-UNDO.
DEF VAR vFolderName  AS CHAR   NO-UNDO. 
DEF VAR vDate        AS DATE   NO-UNDO.
DEF VAR tmpFileName  AS CHAR   NO-UNDO.


/* Значения полей договоров */
DEF TEMP-TABLE ttDog NO-UNDO
   FIELD DogNum     AS CHAR    /* Номер договора */
   FIELD NameZaem   AS CHAR    /* Наименование заёмщика */
   FIELD FopDate    AS DATE    /* Дата заключения договра залога */
   FIELD ObespNum   AS CHAR    /* Номер договора обеспечения */
   FIELD ObespChar  AS CHAR    /* Значения дополнительных реквизитов "ТипДогОб" и "ВидОб" */
   FIELD KachObesp  AS CHAR    /* Категория качества на дату отчета */
   FIELD Poruch     AS CHAR    /* Залогодатель (поручитель) */
   FIELD SumObesp   AS DEC     /* Стоимость по договору */
   FIELD ObespCheck AS CHAR    /* Проверка залога */
   FIELD EqualOb    AS CHAR    /* Справедливая стоимость обеспечения */
   FIELD Pz         AS CHAR    /* Последний залог */

   FIELD BKPKat     AS CHAR    /* БКПрогКат */
   FIELD FC         AS CHAR    /* ФЦельПрог */
   FIELD PK         AS CHAR    /* ПрогКред */
.

{getpath.fun &NO-ask = "YES"}

ASSIGN
   vFolderName = fGetSetting("PreView","SaveFile","./")
   vFolderName = SUBSTRING(vFolderName,1,R-INDEX(vFolderName,"/"))
.
RUN getpath.p (INPUT-OUTPUT vFolderName).
IF vFolderName EQ ? THEN RETURN.
IF CheckExistFolder(RIGHT-TRIM(vFolderName,"/")) NE 0 
THEN DO:
   RUN Fill-SysMes IN h_tmess ("","","-1","Невозможно создать указаный каталог").
   RETURN.
END.

vDate = TODAY.
DO TRANSACTION ON ERROR UNDO, RETRY ON ENDKEY UNDO, LEAVE WITH FRAME dateframe2:
   PAUSE 0 .
   UPDATE
      vDate FORMAT "99/99/9999" LABEL  "На дату"
      WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COLUMNS
      COLOR MESSAGE TITLE "[ ЗАДАЙТЕ ДАТУ ]"
   EDITING:
       READKEY.
       APPLY LASTKEY.
   END.
   ASSIGN vDate.
END.

/* конвертирование строк */
FUNCTION Chr-Conv RETURNS CHARACTER (INPUT iStr AS CHARACTER).
   RETURN CODEPAGE-CONVERT (iStr,  "1251", SESSION:CHARSET).
END FUNCTION.

/* оставляем только договора, на которых ведется резерв */
CASE iParam:
   WHEN "Ю" THEN
   DO:
      FOR EACH tmprecid,
         EACH cust-corp WHERE
            RECID(cust-corp) EQ tmprecid.id
      NO-LOCK:
         vTmpStr = cust-corp.name-corp. 
         
         {66clientgua.i "'Ю'"
                      "cust-corp.cust-id"}
      END.
   END.
   WHEN "Ч" THEN
   DO:
      FOR EACH tmprecid,
         EACH person WHERE
            RECID(person) EQ tmprecid.id
      NO-LOCK:
         vTmpStr = person.name-last + " " + person.first-names.
         
         {66clientgua.i "'Ч'"
                      "person.person-id"}
      END.
   END.
END CASE.

RUN Gen_FileName(OUTPUT tmpFileName). 
RUN out_report_to_file(tmpFileName,iParam).

/* Определение имени файла */
PROCEDURE Gen_FileName.
   DEFINE OUTPUT PARAM oFileName AS CHARACTER.
   oFileName   = TRIM(vFolderName)
               + "clientgua_" + REPLACE(STRING(TODAY), "/", "_") + ".csv".
END PROCEDURE.

/* формирование отчета */
PROCEDURE out_report_to_file.
   DEFINE INPUT PARAM iFileName AS CHARACTER.
   DEFINE INPUT PARAM iParam    AS CHARACTER. 
   
   tmpFileName = SEARCH(iFileName). 
   IF (tmpFileName <> ?) THEN DO:
      RUN Fill-SysMes IN h_tmess("", "", "4", "Файл: " + iFileName + " уже существует\nХотите перезаписать файл?").
      IF (pick-value = "NO") OR (pick-value = "?") THEN DO:
         UPDATE iFileName FORMAT "x(40)" WITH FRAME a ROW 10 SIDE-LABELS CENTERED.
      END.
   END.
   
   OUTPUT TO VALUE(iFileName).

   PUT UNFORMATTED
      Chr-Conv("Отчет по договорам обеспечения на ") vDate SKIP.
      
   FOR EACH ttDog 
   NO-LOCK
   BREAK BY ttDog.NameZaem
         BY ttDog.DogNum
         BY ttDog.ObespNum:

      IF FIRST-OF(ttDog.NameZaem) THEN
         PUT UNFORMATTED
            SKIP (2)
            Chr-Conv("Заемщик: ") Chr-Conv(ttDog.NameZaem) SKIP
            Chr-Conv("Номер кредитного договора;Дата заключения договора залога;Номер договора обеспечения;Характеристика обеспечения;Категория качества;Залогодатель (поручитель);Стоимость по договору;Проверка залога;Справедливая стоимость обеспечения;Последний залог;БКПрогКат;ФЦельПрог;ПрогКред") SKIP.
      
      PUT UNFORMATTED
         Chr-Conv(ttDog.DogNum) ";" ttDog.FopDate ";" Chr-Conv(ttDog.ObespNum) ";" Chr-Conv(ttDog.ObespChar) ";" Chr-Conv(IF ttDog.KachObesp EQ "?" OR ttDog.KachObesp EQ ? THEN "" ELSE ttDog.KachObesp) ";" Chr-Conv(ttDog.Poruch) ";" ttDog.SumObesp ";" Chr-Conv(ttDog.ObespCheck) ";" Chr-Conv(ttDog.EqualOb) ";" Chr-Conv(ttDog.Pz) ";" Chr-Conv(ttDog.BKPKat) ";" Chr-Conv(ttDog.FC) ";" Chr-Conv(ttDog.PK) ";" SKIP.
   END.
   OUTPUT CLOSE.
END PROCEDURE.

RUN Fill-SysMes IN h_tmess ("", "", "", "Файл(ы) сформирован(ы) в каталоге " + vFolderName).

{intrface.del}
