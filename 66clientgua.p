/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: CLIENTGUA.P
      Comment: 0131666 ����⨢�� ���� �� ������ࠬ ���ᯥ祭�� �뤥������ �����⮢
   Parameters:"�","�" 
         Uses:
      Used by:
      Created: 19.10.2010 12:33 feok    
     Modified: 19.10.2010 12:33 feok    
*/

{globals.i}
{intrface.get i254} 
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{tmprecid.def}          /* ������ � ��࠭�묨 �����ﬨ. */

DEF INPUT PARAM iParam AS CHAR NO-UNDO.

DEF VAR vTmpStr      AS CHAR   NO-UNDO.
DEF VAR mSurr        AS CHAR   NO-UNDO.
DEF VAR vFolderName  AS CHAR   NO-UNDO. 
DEF VAR vDate        AS DATE   NO-UNDO.
DEF VAR tmpFileName  AS CHAR   NO-UNDO.


/* ���祭�� ����� ������஢ */
DEF TEMP-TABLE ttDog NO-UNDO
   FIELD DogNum     AS CHAR    /* ����� ������� */
   FIELD NameZaem   AS CHAR    /* ������������ ���騪� */
   FIELD FopDate    AS DATE    /* ��� �����祭�� ������ ������ */
   FIELD ObespNum   AS CHAR    /* ����� ������� ���ᯥ祭�� */
   FIELD ObespChar  AS CHAR    /* ���祭�� �������⥫��� ४����⮢ "��������" � "�����" */
   FIELD KachObesp  AS CHAR    /* ��⥣��� ����⢠ �� ���� ���� */
   FIELD Poruch     AS CHAR    /* ��������⥫� (�����⥫�) */
   FIELD SumObesp   AS DEC     /* �⮨����� �� �������� */
   FIELD ObespCheck AS CHAR    /* �஢�ઠ ������ */
   FIELD EqualOb    AS CHAR    /* ��ࠢ������� �⮨����� ���ᯥ祭�� */
   FIELD Pz         AS CHAR    /* ��᫥���� ����� */

   FIELD BKPKat     AS CHAR    /* ���ண��� */
   FIELD FC         AS CHAR    /* �����ண */
   FIELD PK         AS CHAR    /* �ண�। */
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
   RUN Fill-SysMes IN h_tmess ("","","-1","���������� ᮧ���� 㪠���� ��⠫��").
   RETURN.
END.

vDate = TODAY.
DO TRANSACTION ON ERROR UNDO, RETRY ON ENDKEY UNDO, LEAVE WITH FRAME dateframe2:
   PAUSE 0 .
   UPDATE
      vDate FORMAT "99/99/9999" LABEL  "�� ����"
      WITH CENTERED ROW 10 OVERLAY SIDE-LABELS 1 COLUMNS
      COLOR MESSAGE TITLE "[ ������� ���� ]"
   EDITING:
       READKEY.
       APPLY LASTKEY.
   END.
   ASSIGN vDate.
END.

/* �������஢���� ��ப */
FUNCTION Chr-Conv RETURNS CHARACTER (INPUT iStr AS CHARACTER).
   RETURN CODEPAGE-CONVERT (iStr,  "1251", SESSION:CHARSET).
END FUNCTION.

/* ��⠢�塞 ⮫쪮 �������, �� ������ ������� १�� */
CASE iParam:
   WHEN "�" THEN
   DO:
      FOR EACH tmprecid,
         EACH cust-corp WHERE
            RECID(cust-corp) EQ tmprecid.id
      NO-LOCK:
         vTmpStr = cust-corp.name-corp. 
         
         {66clientgua.i "'�'"
                      "cust-corp.cust-id"}
      END.
   END.
   WHEN "�" THEN
   DO:
      FOR EACH tmprecid,
         EACH person WHERE
            RECID(person) EQ tmprecid.id
      NO-LOCK:
         vTmpStr = person.name-last + " " + person.first-names.
         
         {66clientgua.i "'�'"
                      "person.person-id"}
      END.
   END.
END CASE.

RUN Gen_FileName(OUTPUT tmpFileName). 
RUN out_report_to_file(tmpFileName,iParam).

/* ��।������ ����� 䠩�� */
PROCEDURE Gen_FileName.
   DEFINE OUTPUT PARAM oFileName AS CHARACTER.
   oFileName   = TRIM(vFolderName)
               + "clientgua_" + REPLACE(STRING(TODAY), "/", "_") + ".csv".
END PROCEDURE.

/* �ନ஢���� ���� */
PROCEDURE out_report_to_file.
   DEFINE INPUT PARAM iFileName AS CHARACTER.
   DEFINE INPUT PARAM iParam    AS CHARACTER. 
   
   tmpFileName = SEARCH(iFileName). 
   IF (tmpFileName <> ?) THEN DO:
      RUN Fill-SysMes IN h_tmess("", "", "4", "����: " + iFileName + " 㦥 �������\n���� ��१������ 䠩�?").
      IF (pick-value = "NO") OR (pick-value = "?") THEN DO:
         UPDATE iFileName FORMAT "x(40)" WITH FRAME a ROW 10 SIDE-LABELS CENTERED.
      END.
   END.
   
   OUTPUT TO VALUE(iFileName).

   PUT UNFORMATTED
      Chr-Conv("���� �� ������ࠬ ���ᯥ祭�� �� ") vDate SKIP.
      
   FOR EACH ttDog 
   NO-LOCK
   BREAK BY ttDog.NameZaem
         BY ttDog.DogNum
         BY ttDog.ObespNum:

      IF FIRST-OF(ttDog.NameZaem) THEN
         PUT UNFORMATTED
            SKIP (2)
            Chr-Conv("����騪: ") Chr-Conv(ttDog.NameZaem) SKIP
            Chr-Conv("����� �।�⭮�� �������;��� �����祭�� ������� ������;����� ������� ���ᯥ祭��;��ࠪ���⨪� ���ᯥ祭��;��⥣��� ����⢠;��������⥫� (�����⥫�);�⮨����� �� ��������;�஢�ઠ ������;��ࠢ������� �⮨����� ���ᯥ祭��;��᫥���� �����;���ண���;�����ண;�ண�।") SKIP.
      
      PUT UNFORMATTED
         Chr-Conv(ttDog.DogNum) ";" ttDog.FopDate ";" Chr-Conv(ttDog.ObespNum) ";" Chr-Conv(ttDog.ObespChar) ";" Chr-Conv(IF ttDog.KachObesp EQ "?" OR ttDog.KachObesp EQ ? THEN "" ELSE ttDog.KachObesp) ";" Chr-Conv(ttDog.Poruch) ";" ttDog.SumObesp ";" Chr-Conv(ttDog.ObespCheck) ";" Chr-Conv(ttDog.EqualOb) ";" Chr-Conv(ttDog.Pz) ";" Chr-Conv(ttDog.BKPKat) ";" Chr-Conv(ttDog.FC) ";" Chr-Conv(ttDog.PK) ";" SKIP.
   END.
   OUTPUT CLOSE.
END PROCEDURE.

RUN Fill-SysMes IN h_tmess ("", "", "", "����(�) ��ନ஢��(�) � ��⠫��� " + vFolderName).

{intrface.del}
