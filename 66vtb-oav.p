/*             ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2002 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: vtb-oav.p
      Comment: ����饭�� �� ����⨨ ��⮢ 
         Uses:
      Created: alvel 21.10.2002
     Modified:

qq  ����  �� ����
qq �������� ��� ���ᥫ�宧����  ��᫥ � ��� �..
qq � ����奬� �� branch.�����玎 ���ਫ� �ଠ� � 65 �� 70, �.�. �� �室�� ���祭�� 
   sirotrin - ������� ������ �ନ஢���� ��� ��� ��
*/

{globals.i}
{bank-id.i}
{intrface.get cust}

{tmprecid.def}

&SCOP OFFSET_VAL 480 /* ᤢ�� �� ���孥�� ��� ��࠭��� */

DEFINE STREAM fil.
DEFINE STREAM mfil.
DEFINE STREAM macro-file.

DEFINE BUFFER xprinter FOR PRINTER.

DEFINE VARIABLE account-date   AS DATE                     NO-UNDO. /* ��� ������ ��� */
DEFINE VARIABLE account        AS CHARACTER FORMAT 'x(20)' NO-UNDO. /* ����� ��� */
DEFINE VARIABLE name-client    as CHARACTER FORMAT 'x(71)' NO-UNDO. /* ������ - �������� ��� */
DEFINE VARIABLE current-date   AS DATE                     NO-UNDO. /* ⥪��� �������ୠ� ��� */
DEFINE VARIABLE account-target AS CHARACTER                NO-UNDO. /* 楫� ��� (���.४�����) */
DEFINE VARIABLE rkc-name       AS CHARACTER                NO-UNDO. /* �������� ��� */
DEFINE VARIABLE acct-place     AS CHARACTER                NO-UNDO. /* ���� ������ ��� */
DEFINE VARIABLE chief-oo       AS CHARACTER                NO-UNDO. /* ��砫쭨� ����樮����� �⤥�� */
DEFINE VARIABLE addr           AS CHARACTER                NO-UNDO. /* ��. ���� */
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
   L-Sh   = 180  /* ᬥ�-� �� ���.��� */
   s-leng = 20   /* �ਭ� ᨬ���� */
   Sec    = 1400 /* ��砫� (ᬥ饭��) ��ண� ������� */
.

{setdest.i &stream="stream fil " &cols = 68}
{get_set2.i "�ਭ��" "PCL" "w/o chek"}

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
   
   /* ����饭�� �������� ��� ����� � �ਤ��᪨� ��� */
   IF NOT (acct.cust-cat EQ "�" OR acct.cust-cat EQ "�") THEN 
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
      account-target = GetXattrValue("acct",STRING(acct.acct + "," + acct.currency),"�����")
   .
   
   FIND FIRST branch WHERE branch.branch-id EQ dept.branch NO-LOCK NO-ERROR.
   IF NOT AVAIL branch THEN RETURN.  

   {get_set.i "����_��"}
   ASSIGN
      acct-place = GetXattrValue("branch",dept.branch,"���������")
      addr       = setting.val
      rkc-name   = {banknm.lf banks}
      chief-oo   = GetXattrValue("branch",dept.branch,"�����玎")
      mOGRN      = GetXattrValue("branch",dept.branch,"����")
      mOGRN      = IF {assigned mOGRN}
                   THEN " ���� " + mOGRN
                   ELSE ""
   .
   /* SIR */
   
   IF shFilial = "0000" THEN DO:
      mKpp = FGetSetting("���","������",?).
   END.
   IF NOT {assigned mKPP}  THEN DO:
      mKPP       = GetXattrValue("branch",dept.branch,"���").
   END.
   mKPP       = IF {assigned mKPP} THEN "/��� " + mKPP ELSE "".

   DO mi=1 TO 2:
      PUT STREAM fil SKIP (5).
      PUT STREAM fil UNFORMATTED TRIM(STRING(name-client,'x(81)')) SKIP (1).
      PUT STREAM fil SKIP (5).
      PUT STREAM fil UNFORMATTED
      "����頥�, �� " + STRING(account-date,'99/99/9999') + " ��� ����� " + account-target SKIP.
      PUT STREAM fil UNFORMATTED "��� � " + string(account,'x(20)') + " �� �������� " SKIP(1).
      PUT STREAM fil UNFORMATTED "      " + STRING(acct-place) + ":" SKIP .
      PUT STREAM fil UNFORMATTED "      " + STRING(addr) SKIP.       /* �ਤ��᪨� ����     */
      PUT STREAM fil UNFORMATTED "      ��� " + STRING(bank-mfo-9).   /* ��� �����             */
      PUT STREAM fil UNFORMATTED ", ��� " + STRING(bank-inn) + mKPP + mOGRN SKIP.    /* ��� �����             */
      PUT STREAM fil UNFORMATTED "      �/� " + STRING(bank-acct) SKIP.   /* ����. ���       */ 
      PUT STREAM fil UNFORMATTED "      � " + STRING(rkc-name) + "." SKIP (1).
      put STREAM fil unformatted "�������⥫쭮 ᮮ�頥�, �� � ᮮ⢥��⢨� � �������� �����ᮬ ��" SKIP.
      put STREAM fil unformatted "����ࠫ�� ������� 212-��, ��� ����室��� �  7-�  ������ �ப" SKIP.
      put STREAM fil unformatted "㢥������ �������� �࣠��, ���ᨮ��� 䮭�, ���� ���.���客���� ��" SKIP.
      put STREAM fil unformatted "����⨨ ���㪠������� ���." SKIP (1).
      put STREAM fil unformatted "�� " + STRING(current-date,'99/99/9999') SKIP (1).   
      PUT STREAM fil UNFORMATTED STRING(chief-oo) SKIP (1).
      put STREAM fil UNFORMATTED
      "--------------------------------------------------------------------" SKIP (1).
   END.
   PAGE STREAM fil.

   IF macro-prn THEN DO:
      buf-str = "".
      DO i=1 TO 2:
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 50 + {&OFFSET_VAL},  TRIM(string(name-client,'x(81)')),INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 350 + {&OFFSET_VAL}, "����頥�, �� " + STRING(account-date,'99/99/9999') + " ��� ����� " + account-target, INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 400 + {&OFFSET_VAL}, "��� � " + string(account,'x(20)') + " � ��� ���ᥫ�宧���� " + STRING(acct-place) + ":", INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 550 + {&OFFSET_VAL}, "      " + string(addr) , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 600 + {&OFFSET_VAL}, "      ��� " + string(bank-mfo-9) + ", ��� " + string(bank-inn) + mKPP + mOGRN, INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 650 + {&OFFSET_VAL}, "      �/� " + string(bank-acct) , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 700 + {&OFFSET_VAL}, "      � " + STRING(rkc-name) + "." , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 800 + {&OFFSET_VAL}, "�������⥫쭮 ᮮ�頥�, �� � ᮮ⢥��⢨� � �������� �����ᮬ ��" , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 850 + {&OFFSET_VAL}, "��� ����室��� �  7-�  ������ �ப 㢥������ �������� �࣠�� ��" , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 900 + {&OFFSET_VAL}, "����⨨ ���㪠������� ���." , INPUT-OUTPUT buf-str).
         RUN PUT_PCL_STR(L-Sh, (i - 1) * Sec + 1000 + {&OFFSET_VAL}, "�� " + string(current-date,'99/99/9999') , INPUT-OUTPUT buf-str).
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
