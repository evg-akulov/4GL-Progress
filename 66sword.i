/*-----------------------------------------------------------------------------
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2001 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: sword.i
      Comment: ����� ᢮����� ����ਠ�쭮�� �थ� (�ᯮ������ 䨫���).
               ����頥� ��室 �㬠��. ������砥��� � CTRL-G � ���-�� ���.
   Parameters:
         Uses: sword.p sword-p.p
      Used by:
      Created: 18/06/2002 kraw
     Modified: 26/12/2002 kraw (0013029) ����⠭������ �⮡ࠦ���� ����஢����
     Modified: 16/01/2003 kraw (0011853) �⮣���� �㬬� �ய���� � ����
     Modified: 27/02/2003 kraw (0013230) ����⠭������� ��ਠ�� 㧪�� ����
     Modified: 06/01/2003 kraw (0024627) ���㫥��� ���稪� �⮣���� �㬬� (����室���
             : �� ���� ��᪮�쪨� ������஢)
     Modified: 10/02/2004 kraw (0024321) 3 ����� ��ப� � ���� �थ�
     Modified: 18.05.2006 TSL  ���४�� ����� � ����� 䨫��� ���㬥�⮢
     Modified: 25/07/2006 kraw (0053849) ��ࠢ�⥫�� �थ�
     Modified: 10/02/2010 kraw (0123429) ������⢢� ��ப � detail 㢥��祭� �� 7
     Modified: 07/06/2010 kraw (0122853) ���� ����� ���㬥�� 㢥��祭� �� 10 ���������.
-----------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/
{globals.i}
{pp-uni.var}
{pp-uni.prg}
{intrface.get strng}    /* ������⥪� ��� ࠡ��� � ��ப���. */

DEFINE VARIABLE senderName   AS   CHARACTER EXTENT 7        NO-UNDO.
DEFINE VARIABLE receiverName AS   CHARACTER EXTENT 7        NO-UNDO.
DEFINE VARIABLE sDate        AS   CHARACTER FORMAT "x(29)"  NO-UNDO.  /* ���         */
DEFINE VARIABLE sFilt        AS   CHARACTER FORMAT "x(29)"  NO-UNDO.  /* ��� 䨫���  */
DEFINE VARIABLE acct-db      AS   CHARACTER                 NO-UNDO.
DEFINE VARIABLE acct-cr      AS   CHARACTER                 NO-UNDO.
DEFINE VARIABLE cAmtStr      AS   CHARACTER                 NO-UNDO.
DEFINE VARIABLE cDecStr      AS   CHARACTER                 NO-UNDO.
DEFINE VARIABLE amtstr1      AS   CHARACTER EXTENT 3        NO-UNDO.
DEFINE VARIABLE i-amt-rub    LIKE op-entry.amt-rub  INIT 0  NO-UNDO.
DEFINE VARIABLE i-amt-cur    LIKE op-entry.amt-cur  INIT 0  NO-UNDO.
DEFINE VARIABLE i-amt-qty    LIKE op-entry.qty      INIT 0  NO-UNDO.
DEFINE VARIABLE mAmount      AS   INT64                   NO-UNDO. /* �᫮ �஢���� */
DEFINE VARIABLE i-cur        LIKE op-entry.currency INIT ?  NO-UNDO.
DEFINE VARIABLE mKolStr      AS   INT64                   NO-UNDO.
DEFINE VARIABLE mBegDate     AS   DATE                      NO-UNDO.
DEFINE VARIABLE mEndDate     AS   DATE                      NO-UNDO.
&IF DEFINED(FILE_SWORD_I_NO_POLUPR) <> 0 OR DEFINED(FILE_SWORD_I_COMPRESSED_TOTALS) <> 0 &THEN
   DEFINE VARIABLE mKursDiffAccs AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mCurrency     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mCurOp        AS INT64   NO-UNDO.
   DEFINE VARIABLE mRubFromDeb   AS LOGICAL   NO-UNDO.
&ENDIF

{flt-val.i}
{strtout3.i &cols=95 &custom="printer.page-lines - "}

{tmprecid.def} /* �롨����� ����� �� �� �� RecId) */
DEF TEMP-TABLE totals NO-UNDO /* �⮣� ��� */
    FIELD currency LIKE op-entry.currency
    FIELD amt-rub  LIKE op-entry.amt-rub
    FIELD amt-cur  LIKE op-entry.amt-cur
    FIELD qty      LIKE op-entry.qty
    FIELD cat      AS CHARACTER
    FIELD i        AS   INT64 /* ���-�� �஢���� */.
.

&IF DEFINED(FILE_SWORD_I_NO_POLUPR) <> 0 OR DEFINED(FILE_SWORD_I_COMPRESSED_TOTALS) <> 0 &THEN
DEFINE TEMP-TABLE tmpToIgnore
    FIELD Rwd AS ROWID
INDEX ByRwd IS PRIMARY Rwd.
DEFINE BUFFER xop-entry FOR op-entry.
DEFINE BUFFER xdebacct  FOR acct.
DEFINE BUFFER xcredacct FOR acct.
&ENDIF

&IF DEFINED(rshb) &THEN
DEFINE TEMP-TABLE rshbtt NO-UNDO
   FIELD Cur    AS CHARACTER
   FIELD AmtRub AS DECIMAL
   FIELD AmtCur AS DECIMAL
   FIELD Qty    LIKE op-entry.qty
   .
&ENDIF

{get_set.i "����"}
PUT UNFORMATTED  "&l1S" skip.
PUT UNFORMATTED  "     " setting.val SKIP(2). /* �������� ����� */

ASSIGN
   i-amt-rub = 0
   i-amt-cur = 0
   i-amt-qty = 0
   mBegDate  = IF GetFltVal('op-date1') = ?
               THEN gend-date
               ELSE DATE(GetFltVal('op-date1'))
   mEndDate  = IF GetFltVal('op-date2') = ?    
               THEN gend-date                  
               ELSE DATE(GetFltVal('op-date2'))
   PackagePrint = YES
   sDate = STRING(CAPS({term2str mBegDate mEndDate YES}), "x(30)" )
   sFilt = FStrCenter(IF IsFieldChange("*") 
                      THEN ('������ "' + GetEntries(3,GetFltVal("UserConf"),",","?") + '"') 
                      ELSE "�� ���㬥���",75 )
   &IF DEFINED(FILE_SWORD_I_NO_POLUPR) <> 0 OR DEFINED(FILE_SWORD_I_COMPRESSED_TOTALS) <> 0 &THEN
      mKursDiffAccs = FGetSetting("���������",?,"") + "," + FGetSetting("���������",?,"")
   &ENDIF
.

&IF DEFINED( FILE_sword_i_wide ) NE 0 &THEN
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" skip.
 &IF DEFINED(sword_i_ispr) EQ 0 &THEN
   PUT UNFORMATTED  "     �        ��������� �஢������� ����権 ��  " + sDate +                  "                                                                                          �"  skip.
 &ELSE
   PUT UNFORMATTED  "     �        ������� ������������ �������������� ����� �� ���������� ��� ��  " + sDate +                  "                                                                           �"  skip.
 &ENDIF
   
   PUT UNFORMATTED  "     �        " + sFilt +                                                               "                                                                                              �"  skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "     ��஢��� �� ����㪠����� ��⠬ ᫥���騥 �����:                                                                                                                               �" skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "     �  ���.N   �        �����       �       �।��       ����.�    �㬬� � ����� � �㬬� � ���.���.  �       ���⥫�騪                      �       �����⥫�                      �" skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.

&ELSEIF DEFINED( FILE_sword_wide_new ) NE 0 &THEN
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" skip.
 &IF DEFINED(sword_i_ispr) EQ 0 &THEN
   PUT UNFORMATTED  "     �        ��������� �஢������� ����権 �� " + sDate +                  "                                                                                          �" skip.
 &ELSE
   PUT UNFORMATTED  "     �        ������� ������������ �������������� ����� �� ���������� ��� ��  " + sDate +                  "                                                                           �" skip.
 &ENDIF
   
   PUT UNFORMATTED  "     �        " + sFilt +                                                               "                                                                                              �" skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "     ��஢��� �� ����㪠����� ��⠬ ᫥���騥 �����:                                                                                                                               �" skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "     �  ���.N   �        �����       �       �।��       ����.��㬬� � ����⥳�㬬� � ���.���.�       ���⥫�騪        �       �����⥫�        �         �����祭�� ���⥦�        �" skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.

&ELSEIF DEFINED(FILE_SWORD_I_NAZN) NE 0 &THEN
   PUT UNFORMATTED  "     ����������������������������������������������������������������������������������������������������������������������������������������������������Ŀ" skip.
 &IF DEFINED(sword_i_ispr) EQ 0 &THEN
   PUT UNFORMATTED  "     �        ��������� �஢������� ����権 ��  " + sDate +                  "                                                             �"  skip.
 &ELSE
   PUT UNFORMATTED  "     �        ������� ������������ �������������� ����� �� ���������� ��� ��  " + sDate +                  "                                              �"  skip.
 &ENDIF
   PUT UNFORMATTED  "     �        " + sFilt +                                                               "                                                                 �"  skip.
   PUT UNFORMATTED  "     ����������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "     ��஢��� �� ����㪠����� ��⠬ ᫥���騥 �����:                                                                                                  �" skip.
   PUT UNFORMATTED  "     ����������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "     �  ���.N   �        �����       �       �।��       ����.�    �㬬� � ����� � �㬬� � ���.���.  � �����祭��                                       �" skip.
   PUT UNFORMATTED  "     ����������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.

&ELSEIF DEFINED(FILE_SWORD_I_RUB) NE 0 &THEN
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������Ŀ" skip.
   PUT UNFORMATTED  "     �        ��������� �஢������� ����権 ��   " + sDate +                  "                                                 �"  skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "     ��஢��� �� ����㪠����� ��⠬ ᫥���騥 �����:                                                                         �" skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "     �  ���.N   �        �����       �       �।��       � �㬬� � ���.���.  � �����祭��                                       �" skip.
   PUT UNFORMATTED  "     ���������������������������������������������������������������������������������������������������������������������������Ĵ" skip.

&ELSE
   PUT UNFORMATTED  "�������������������������������������������������������������������������������������������������Ŀ" skip.
 &IF DEFINED(sword_i_ispr) EQ 0 &THEN
   PUT UNFORMATTED  "�        ������� ������������ ����� �� ���������� ��� ��  " + sDate +                  "          �" skip.
 &ELSE
   PUT UNFORMATTED  "�      ������� ������������ �������������� ����� �� ���������� ��� �� " + STRING(sDate, "x(24)") + "    �" skip.
 &ENDIF
   PUT UNFORMATTED  "�        " + sFilt +                                                               "              �" skip.
   PUT UNFORMATTED  "�������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "��஢��� �� ����㪠����� ��⠬ ᫥���騥 �����:                                               �" skip.
   PUT UNFORMATTED  "�������������������������������������������������������������������������������������������������Ĵ" skip.
   PUT UNFORMATTED  "�  ���.N   �        �����       �       �।��       ����.�    �㬬� � ����� � �㬬� � ���.���.  �" skip.
   PUT UNFORMATTED  "�������������������������������������������������������������������������������������������������Ĵ" skip.
&ENDIF

FOR EACH tmprecid NO-LOCK,

&IF DEFINED( FILE_sword_p ) NE 0 &THEN
   FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK,
   EACH op-entry OF op NO-LOCK
   BREAK &IF DEFINED(SORT-BY) NE 0 &THEN
             {&SORT-BY}
         &ELSE
             BY op-entry.currency BY op-entry.amt-rub
         &ENDIF
:
&ELSE
  FIRST op-entry WHERE RECID(op-entry) = tmprecid.id NO-LOCK,
     FIRST op OF op-entry NO-LOCK BREAK BY op-entry.currency BY op-entry.amt-rub BY op-entry.qty:
&ENDIF

&IF DEFINED(FILE_SWORD_I_NO_POLUPR) NE 0 &THEN
      IF CAN-FIND(FIRST tmpToIgnore WHERE tmpToIgnore.Rwd = ROWID(op-entry) NO-LOCK)
         THEN NEXT.
      IF mCurOp <> op.op THEN ASSIGN
         mCurOp      = op.op
         mRubFromDeb = (IF {assigned op-entry.acct-db} THEN YES ELSE NO)
      .
      ASSIGN
         acct-db = op-entry.acct-db
         acct-cr = op-entry.acct-cr
      .
      RELEASE xop-entry.
      IF op-entry.acct-cr eq ? THEN DO:
         FIND FIRST xop-entry WHERE xop-entry.op      = op.op 
                                AND xop-entry.acct-db = ? NO-LOCK USE-INDEX op-entry NO-ERROR.
         IF AVAIL xop-entry THEN DO:
            CREATE tmpToIgnore.
            ASSIGN
               acct-cr         = xop-entry.acct-cr
               tmpToIgnore.Rwd = ROWID(xop-entry)
            .
         END.
         {find-act.i
            &bact = xdebacct
            &acct = op-entry.acct-db
            &curr = op-entry.currency
         }   
         {find-act.i
            &bact = xcredacct
            &acct = xop-entry.acct-cr
            &curr = xop-entry.currency
         }   
      END.
      ELSE IF op-entry.acct-db EQ ? THEN DO:
         FIND FIRST xop-entry WHERE xop-entry.op      = op.op
                                AND xop-entry.acct-cr = ? NO-LOCK USE-INDEX op-entry NO-ERROR.
         IF AVAIL xop-entry THEN DO:
            CREATE tmpToIgnore.
            ASSIGN
               acct-db         = xop-entry.acct-db
               tmpToIgnore.Rwd = ROWID(xop-entry)
            .
         END.
         {find-act.i
            &bact = xdebacct
            &acct = xop-entry.acct-db
            &curr = xop-entry.currency
         }   
         {find-act.i
            &bact = xcredacct
            &acct = op-entry.acct-cr
            &curr = op-entry.currency
         }   
      END.
      ELSE DO:
         {find-act.i
            &bact = xdebacct
            &acct = op-entry.acct-db
            &curr = op-entry.currency
         }   
         {find-act.i
            &bact = xcredacct
            &acct = op-entry.acct-cr
            &curr = op-entry.currency
         }   
      END.
      IF acct-cr = ? THEN acct-cr = "".
      IF acct-db = ? THEN acct-db = "".

      IF AVAIL xop-entry THEN DO:
         IF (    xdebacct.currency   = ""
             AND xcredacct.currency <> ""
             AND CAN-DO(mKursDiffAccs,xdebacct.acct))
         OR (    xdebacct.currency  <> ""
             AND xcredacct.currency  = ""
             AND CAN-DO(mKursDiffAccs,xcredacct.acct))
         THEN mCurrency = "".
         ELSE DO:
            IF xop-entry.currency <> ""
               THEN mCurrency = xop-entry.currency.
               ELSE mCurrency = op-entry.currency.
         END.
      END.
      ELSE mCurrency = op-entry.currency.
&ELSE
     ASSIGN
        acct-cr =  IF (op-entry.acct-cr <> ?) THEN op-entry.acct-cr ELSE ""
        acct-db =  IF (op-entry.acct-db <> ?) THEN op-entry.acct-db ELSE ""
     .
   &IF DEFINED(SORT-BY) EQ 0 &THEN
     ACCUMULATE op-entry.amt-rub (TOTAL COUNT BY op-entry.currency).
     ACCUMULATE op-entry.amt-cur (TOTAL COUNT BY op-entry.currency).
     ACCUMULATE op-entry.qty (TOTAL COUNT BY op-entry.currency).
   &ENDIF
&ENDIF
   
&IF DEFINED(FILE_sword_i_wide) NE 0 OR DEFINED(FILE_sword_wide_new) NE 0 &THEN
      {empty Info-Store}
      RUN Collection-Info.
      RUN for-pay("�����,����������,������,������,�������",
               "��",
               OUTPUT PlName[1],
               OUTPUT PlLAcct,
               OUTPUT PlRKC[1],
               OUTPUT PlCAcct,
               OUTPUT PlMFO).
      senderName[1] = PlName[1].
      RUN for-rec("������,����������,�������,������,�������",
               "��",
               OUTPUT PoName[1],
               OUTPUT PoAcct,
               OUTPUT PoRKC[1],
               OUTPUT PoCAcct,
               OUTPUT PoMFO).
      receiverName[1] = PoName[1].

      {wordwrap.i &s = PlName &n = 5 &l = 39}
      {wordwrap.i &s = PoName &n = 5 &l = 39}

&ELSEIF DEFINED(FILE_SWORD_I_NAZN) NE 0 &THEN
   RUN DefDetail.
   {wordwrap.i &s = Detail &n = 7 &l = 50}

&ELSEIF DEFINED(FILE_SWORD_I_RUB) NE 0 &THEN
   RUN DefDetail.
   {wordwrap.i &s = Detail &n = 7 &l = 50}
&ENDIF

&IF DEFINED(FILE_sword_wide_new) NE 0 &THEN
   RUN DefDetail.
   {wordwrap.i &s = Detail &n = 7 &l = 35}
&ENDIF

PUT UNFORMATTED
         "     �" + STRING(op.doc-num, "xxxxxxxxxx") +
         "�" + STRING(acct-db, "x(20)")     +
         "�" + STRING(acct-cr, "x(20)")     +
      &IF DEFINED(FILE_SWORD_I_RUB) EQ 0 &THEN
         &IF DEFINED(FILE_SWORD_I_NO_POLUPR) NE 0 &THEN
            "�" + STRING(mCurrency, "x(4)") +
         &ELSE
            "�" + STRING(op-entry.currency, "x(4)") +
         &ENDIF 
      &ENDIF       
&IF DEFINED(FILE_sword_wide_new) EQ 0 &THEN
      &IF DEFINED(FILE_SWORD_I_RUB) EQ 0 &THEN 
            &IF DEFINED(FILE_SWORD_I_NO_POLUPR) NE 0 &THEN
               "�" + ( IF AVAIL xop-entry AND xop-entry.currency = mCurrency THEN STRING(xop-entry.amt-cur, "->>>,>>>,>>>,>>9.99") ELSE STRING(op-entry.amt-cur, "->>>,>>>,>>>,>>9.99") ) +
            &ELSE
               "�" + STRING(op-entry.amt-cur, "->>>,>>>,>>>,>>9.99") +
            &ENDIF
      &ENDIF              
            "�" + (IF op-entry.acct-cat NE "d"
                   THEN
                   &IF DEFINED(FILE_SWORD_I_NO_POLUPR) NE 0 &THEN
                      (IF {assigned op-entry.acct-db} = mRubFromDeb
                       OR NOT AVAIL xop-entry
                          THEN STRING(op-entry.amt-rub, "->>>,>>>,>>>,>>9.99")
                          ELSE STRING(xop-entry.amt-rub, "->>>,>>>,>>>,>>9.99"))
                   &ELSE
                      STRING(op-entry.amt-rub, "->>>,>>>,>>>,>>9.99")
                   &ENDIF
                   ELSE STRING(op-entry.qty, "->>,>>>,>>9.9999999")).
&ELSE
      &IF DEFINED(FILE_SWORD_I_RUB) EQ 0 &THEN 
            &IF DEFINED(FILE_SWORD_I_NO_POLUPR) NE 0 &THEN
               "�" + ( IF AVAIL xop-entry AND xop-entry.currency = mCurrency THEN STRING(xop-entry.amt-cur, ">>>>>>>>>>9.99") ELSE STRING(op-entry.amt-cur, ">>>>>>>>>>9.99") ) +
            &ELSE
               "�" + STRING(op-entry.amt-cur, ">>>>>>>>>>9.99") +
            &ENDIF
      &ENDIF              
            "�" + (IF op-entry.acct-cat NE "d"
                   THEN
                   &IF DEFINED(FILE_SWORD_I_NO_POLUPR) NE 0 &THEN
                      (IF {assigned op-entry.acct-db} = mRubFromDeb
                       OR NOT AVAIL xop-entry
                          THEN STRING(op-entry.amt-rub, ">>>>>>>>>>>>9.99")
                          ELSE STRING(xop-entry.amt-rub, ">>>>>>>>>>>>9.99"))
                   &ELSE
                      STRING(op-entry.amt-rub, ">>>>>>>>>>>>9.99")
                   &ENDIF
                   ELSE STRING(op-entry.qty, ">>>>>>>9.9999999")).
&ENDIF

&IF DEFINED(rshb) NE 0 &THEN
   CREATE rshbtt.
   ASSIGN
      rshbtt.AmtCur = op-entry.amt-cur
      rshbtt.AmtRub = op-entry.amt-rub
      rshbtt.Qty    = op-entry.qty
      rshbtt.Cur    = op-entry.currency
   .
&ENDIF

&IF DEFINED( FILE_sword_i_wide ) NE 0 &THEN
   PUT UNFORMATTED
         "�" + STRING(PlName[1], "x(39)") +
         "�" + STRING(PoName[1], "x(39)").
   DO mKolStr = 2 TO 5:
      IF PlName[mKolStr] NE "" OR POName[mKolStr] NE "" THEN
         PUT UNFORMATTED "�" SKIP "�          �                    �                    �    �                   �                   �" 
             + STRING(PlName[mKolStr],"x(39)") + "�" + STRING(POName[mKolStr],"x(39)").
   END.
&ELSEIF DEFINED( FILE_sword_wide_new ) NE 0 &THEN
   IF senderName[1] BEGINS "���" THEN 
      senderName[1] = SUBSTRING(senderName[1],INDEX(senderName[1]," ",5)).
   {wordwrap.i &s = senderName
               &n = 7
               &l = 25
   }
   IF receiverName[1] BEGINS "���" THEN 
      receiverName[1] = SUBSTRING(receiverName[1],INDEX(receiverName[1]," ",5)).
   {wordwrap.i &s = receiverName
               &n = 7
               &l = 25
   }

   PUT UNFORMATTED  "�" + STRING(senderName[1],   "x(25)") +
                    "�" + STRING(receiverName[1], "x(25)") + 
                    "�" + STRING(Detail[1],"x(35)").

   DO mKolStr = 2 TO 7:
      IF Detail[mKolStr] NE "" OR senderName[mKolStr] NE "" OR receiverName[mKolStr] NE "" THEN
         PUT UNFORMATTED "�" SKIP "     �          �                    �                    �    �              �                �" + 
            STRING(senderName[mKolStr],"x(25)") + "�" + STRING(receiverName[mKolStr],"x(25)") + "�" + STRING(Detail[mKolStr],"x(35)").
   END.

&ELSEIF DEFINED(FILE_SWORD_I_NAZN) NE 0 &THEN
   PUT UNFORMATTED "�" STRING(Detail[1],"x(50)").
   DO mKolStr = 2 TO 7:
      IF Detail[mKolStr] NE "" THEN
         PUT UNFORMATTED "�" SKIP "     �          �                    �                    �    �                   �                   �" + STRING(Detail[mKolStr],"x(50)").
   END.

&ELSEIF DEFINED(FILE_SWORD_I_RUB) NE 0 &THEN
   PUT UNFORMATTED "�" STRING(Detail[1],"x(50)").
   DO mKolStr = 2 TO 7:
      IF Detail[mKolStr] NE "" THEN
         PUT UNFORMATTED "�" SKIP "     �          �                    �                    �                   �" + STRING(Detail[mKolStr],"x(50)").
   END.

&ENDIF

PUT UNFORMATTED "�" SKIP.

&IF DEFINED(SORT-BY) NE 0 &THEN
   ASSIGN
      i-amt-rub = i-amt-rub + op-entry.amt-rub
      i-amt-cur = i-amt-cur + op-entry.amt-cur
      i-amt-qty = i-amt-qty + op-entry.qty
   .
   IF i-cur = ? THEN i-cur = op-entry.currency.
   ELSE IF i-cur <> op-entry.currency THEN i-cur = "".
&ELSE
   &IF DEFINED(FILE_SWORD_I_NO_POLUPR) NE 0 &THEN
      FIND FIRST totals WHERE totals.currency = mCurrency EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL totals THEN DO:
         CREATE totals.
         ASSIGN
            totals.currency = mCurrency
            totals.cat      = op-entry.acct-cat
         .
      END.

      IF  AVAIL xop-entry
      AND xop-entry.currency = mCurrency
      THEN totals.amt-cur = totals.amt-cur + xop-entry.amt-cur.
      ELSE totals.amt-cur = totals.amt-cur + op-entry.amt-cur.
      IF {assigned op-entry.acct-db} = mRubFromDeb
      OR NOT AVAIL xop-entry
         THEN totals.amt-rub = totals.amt-rub + op-entry.amt-rub.
         ELSE totals.amt-rub = totals.amt-rub + xop-entry.amt-rub.
      ASSIGN
         totals.qty = totals.qty + op-entry.qty
         totals.i   = totals.i   + 1
      .
   &ELSE
   &IF DEFINED(FILE_SWORD_I_COMPRESSED_TOTALS) = 0 &THEN
    IF LAST-OF(op-entry.currency) THEN DO:
       CREATE totals.
       ASSIGN
          totals.currency = op-entry.currency
          totals.amt-rub  = (ACCUM TOTAL BY op-entry.currency op-entry.amt-rub)
          totals.amt-cur  = (ACCUM TOTAL BY op-entry.currency op-entry.amt-cur)
          totals.qty      = (ACCUM TOTAL BY op-entry.currency op-entry.qty)
          totals.cat      = op-entry.acct-cat
          totals.i        = (ACCUM COUNT BY op-entry.currency op-entry.amt-rub)
       .
    END. /* last-of */
   &ENDIF
   &ENDIF
&ENDIF

END.

&IF DEFINED(FILE_SWORD_I_COMPRESSED_TOTALS) <> 0 &THEN
   FOR EACH tmprecid NO-LOCK,
   &IF DEFINED( FILE_sword_p ) NE 0 &THEN
      FIRST op WHERE RECID(op) = tmprecid.id NO-LOCK,
      EACH op-entry OF op NO-LOCK BREAK BY op-entry.currency BY op-entry.amt-rub:
   &ELSE
      FIRST op-entry WHERE RECID(op-entry) = tmprecid.id NO-LOCK,
      FIRST op OF op-entry NO-LOCK BREAK BY op-entry.currency BY op-entry.amt-rub BY op-entry.qty:
   &ENDIF

      IF CAN-FIND(FIRST tmpToIgnore WHERE tmpToIgnore.Rwd = ROWID(op-entry) NO-LOCK)
         THEN NEXT.
      IF mCurOp <> op.op THEN ASSIGN
         mCurOp      = op.op
         mRubFromDeb = (IF {assigned op-entry.acct-db} THEN YES ELSE NO)
      .
      RELEASE xop-entry.
      IF op-entry.acct-cr eq ? THEN DO:
         FIND FIRST xop-entry WHERE xop-entry.op      = op.op 
                                AND xop-entry.acct-db = ? NO-LOCK USE-INDEX op-entry NO-ERROR.
         IF AVAIL xop-entry THEN DO:
            CREATE tmpToIgnore.
            tmpToIgnore.Rwd = ROWID(xop-entry).
         END.
         {find-act.i
            &bact = xdebacct
            &acct = op-entry.acct-db
            &curr = op-entry.currency
         }   
         {find-act.i
            &bact = xcredacct
            &acct = xop-entry.acct-cr
            &curr = xop-entry.currency
         }   
      END.
      ELSE IF op-entry.acct-db EQ ? THEN DO:
         FIND FIRST xop-entry WHERE xop-entry.op      = op.op
                                AND xop-entry.acct-cr = ? NO-LOCK USE-INDEX op-entry NO-ERROR.
         IF AVAIL xop-entry THEN DO:
            CREATE tmpToIgnore.
            tmpToIgnore.Rwd = ROWID(xop-entry).
         END.
         {find-act.i
            &bact = xdebacct
            &acct = xop-entry.acct-db
            &curr = xop-entry.currency
         }   
         {find-act.i
            &bact = xcredacct
            &acct = op-entry.acct-cr
            &curr = op-entry.currency
         }   
      END.
      ELSE DO:
         {find-act.i
            &bact = xdebacct
            &acct = op-entry.acct-db
            &curr = op-entry.currency
         }   
         {find-act.i
            &bact = xcredacct
            &acct = op-entry.acct-cr
            &curr = op-entry.currency
         }   
      END.

      IF AVAIL xop-entry THEN DO:
         IF (    xdebacct.currency   = ""
             AND xcredacct.currency <> ""
             AND CAN-DO(mKursDiffAccs,xdebacct.acct))
         OR (    xdebacct.currency  <> ""
             AND xcredacct.currency  = ""
             AND CAN-DO(mKursDiffAccs,xcredacct.acct))
         THEN mCurrency = "".
         ELSE DO:
            IF xop-entry.currency <> ""
               THEN mCurrency = xop-entry.currency.
               ELSE mCurrency = op-entry.currency.
         END.
      END.
      ELSE mCurrency = op-entry.currency.

      FIND FIRST totals WHERE totals.currency = mCurrency EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL totals THEN DO:
         CREATE totals.
         ASSIGN
            totals.currency = mCurrency
            totals.cat      = op-entry.acct-cat
         .
      END.

      IF  AVAIL xop-entry
      AND xop-entry.currency = mCurrency
      THEN totals.amt-cur = totals.amt-cur + xop-entry.amt-cur.
      ELSE totals.amt-cur = totals.amt-cur + op-entry.amt-cur.
      IF {assigned op-entry.acct-db} = mRubFromDeb
      OR NOT AVAIL xop-entry
         THEN totals.amt-rub = totals.amt-rub + op-entry.amt-rub.
         ELSE totals.amt-rub = totals.amt-rub + xop-entry.amt-rub.
      ASSIGN
         totals.qty = totals.qty + op-entry.qty
         totals.i   = totals.i   + 1
      .
   END. /* FOR EACH tmprecid */
&ENDIF

&IF DEFINED(rshb) &THEN
FOR EACH rshbtt NO-LOCK BREAK BY rshbtt.Cur BY rshbtt.AmtRub:
   ACCUMULATE rshbtt.AmtRub (TOTAL COUNT BY rshbtt.Cur).
   ACCUMULATE rshbtt.AmtCur (TOTAL COUNT BY rshbtt.Cur).
   ACCUMULATE rshbtt.Qty    (TOTAL COUNT BY rshbtt.Cur).
   IF LAST-OF(rshbtt.Cur) THEN DO:
      CREATE totals.
      ASSIGN
         totals.currency = rshbtt.Cur
         totals.amt-rub  = (ACCUM TOTAL BY rshbtt.Cur rshbtt.AmtRub)
         totals.amt-cur  = (ACCUM TOTAL BY rshbtt.Cur rshbtt.AmtCur)
         totals.qty      = (ACCUM TOTAL BY rshbtt.Cur rshbtt.Qty)
         totals.i        = (ACCUM COUNT BY rshbtt.Cur rshbtt.AmtRub)
       .
    END. /* last-of */
END.
&ENDIF

&IF DEFINED(SORT-BY) EQ 0 OR DEFINED(rshb) NE 0 &THEN

   &IF DEFINED( FILE_sword_i_wide ) NE 0 &THEN
   PUT UNFORMATTED "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.

   &ELSEIF DEFINED( FILE_sword_wide_new ) NE 0 &THEN
   PUT UNFORMATTED "     ���������������������������������������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.

   &ELSEIF DEFINED(FILE_SWORD_I_NAZN) NE 0 &THEN
   PUT UNFORMATTED "     ����������������������������������������������������������������������������������������������������������������������������������������������������Ĵ" skip.

   &ELSEIF DEFINED(FILE_SWORD_I_RUB) NE 0 &THEN
   PUT UNFORMATTED "     ���������������������������������������������������������������������������������������������������������������������������Ĵ" skip.

   &ELSE
   PUT UNFORMATTED "     �������������������������������������������������������������������������������������������������Ĵ" skip.
   &ENDIF

&ENDIF /* &IF DEFINED(SORT-BY) EQ 0 OR DEFINED(rshb) NE 0 &THEN */

FOR EACH totals:

&IF DEFINED(FILE_SWORD_I_RUB) EQ 0 &THEN
    &IF DEFINED(FILE_sword_wide_new) NE 0  &THEN
       PUT UNFORMATTED  "     � �⮣�: " + STRING(totals.i, ">>>>9") + " �஢.  " + "                            " + string(totals.currency, "xxxx") + " " + string(totals.amt-cur,  ">>>>>>>>>>9.99") + " " + (IF totals.cat NE "d" THEN STRING(totals.amt-rub, ">>>>>>>>>>>>9.99")
                                                                                                                                                                                                                                ELSE STRING(totals.qty, ">>>>>>>9.9999999")) +
    &ELSE
       PUT UNFORMATTED  "     � �⮣�: " + STRING(totals.i, ">>>>9") + " �஢.  " + "                            " + string(totals.currency, "xxxx") + " " + string(totals.amt-cur,  "->>>,>>>,>>>,>>9.99") + " " + (IF totals.cat NE "d" THEN STRING(totals.amt-rub, "->>>,>>>,>>>,>>9.99")
                                                                                                                                                                                                                                     ELSE STRING(totals.qty, "->>,>>>,>>9.9999999")) +
    &ENDIF

    &IF DEFINED( FILE_sword_i_wide ) NE 0 &THEN 
       FILL(" ",80) +
    
    &ELSEIF DEFINED(FILE_sword_wide_new) NE 0 &THEN
       FILL(" ",88) +
    
    &ELSEIF DEFINED(FILE_SWORD_I_NAZN) NE 0 &THEN
       FILL(" ",51) +
    &ENDIF
    
       "    �" SKIP.
&ENDIF

&IF DEFINED(SORT-BY) EQ 0 &THEN
   ASSIGN
      i-amt-rub = i-amt-rub + totals.amt-rub
      i-amt-cur = i-amt-cur + totals.amt-cur
      i-amt-qty = i-amt-qty + totals.qty
   .
   IF i-cur = ? THEN i-cur = totals.currency.
   ELSE IF i-cur <> totals.currency THEN i-cur = "".
&ENDIF
mAmount = mAmount + totals.i.
END.     

&IF DEFINED(FILE_SWORD_I_RUB) NE 0 &THEN      
    PUT UNFORMATTED   "     � �⮣�: " + STRING(mAmount, ">>>>9")  + " �஢.  "  + "                            " +  
          STRING(i-amt-rub, "->>>,>>>,>>>,>>9.99")  + FILL(" ",51) +  "    �"SKIP.
&ENDIF

{empty totals}

&IF DEFINED( FILE_sword_i_wide ) NE 0 &THEN
   PUT UNFORMATTED "     �����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������" skip.

&ELSEIF DEFINED( FILE_sword_wide_new ) NE 0 &THEN
   PUT UNFORMATTED "     �����������������������������������������������������������������������������������������������������������������������������������������������������������������������������������" skip.

&ELSEIF DEFINED(FILE_SWORD_I_NAZN) NE 0 &THEN
   PUT UNFORMATTED "     ������������������������������������������������������������������������������������������������������������������������������������������������������" skip.

&ELSEIF DEFINED(FILE_SWORD_I_RUB) NE 0 &THEN
   PUT UNFORMATTED "     �����������������������������������������������������������������������������������������������������������������������������" skip.

&ELSE
   PUT UNFORMATTED "     ���������������������������������������������������������������������������������������������������" skip.
&ENDIF

IF i-cur EQ ""
OR (    i-amt-cur = 0
    AND i-amt-qty = 0) THEN
   RUN x-amtstr.p (i-amt-rub, '', YES, YES,
                   OUTPUT cAmtStr, OUTPUT cDecStr).
ELSE
DO:
   IF i-amt-cur NE 0 THEN
      RUN x-amtstr.p (i-amt-cur, i-cur, YES, YES,
                      OUTPUT cAmtStr, OUTPUT cDecStr).
   ELSE
      RUN x-amtstr.p (i-amt-qty, i-cur, YES, YES,
                      OUTPUT cAmtStr, OUTPUT cDecStr).
END.

AmtStr1 = cAmtStr + ' ' + cDecStr.

&IF DEFINED( FILE_sword_i_wide ) NE 0 OR DEFINED( FILE_sword_wide_new ) NE 0 &THEN
{wordwrap.i &s=amtstr1 &n=3 &l=170}
&ELSE
{wordwrap.i &s=amtstr1 &n=3 &l=90}
&ENDIF

PUT UNFORMATTED  "      �⮣�: "  amtstr1[1]  SKIP.
IF LENGTH(amtstr1[2]) > 0 THEN
   PUT UNFORMATTED  "        "  amtstr1[2]  SKIP.
IF LENGTH(amtstr1[3]) > 0 THEN
   PUT UNFORMATTED  "        "  amtstr1[3]  SKIP.

PUT UNFORMATTED SKIP(1).

PUT UNFORMATTED "     �ਫ������ �� ____ �����." SKIP(2).
PUT UNFORMATTED "     ��壠���"+  FILL("_", 15) +  FILL(" ", 10) + "����஫��" +  FILL("_", 15) SKIP.
/* {signatur.i} */  /* - ������ ���� ��� � ��४�� */
PackagePrint = FALSE.
PUT UNFORMATTED SKIP(3).
{endout3.i}

