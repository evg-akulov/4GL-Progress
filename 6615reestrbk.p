/* ����� ॥��� ���㬥�⮢, ����祭��� �� ����-������� */
/*����ᨡ��᪨� ����, ���ࠢ�� ��� ���. ��ਫ��� �.�. 11.01.11*/
/*� ��� ��� ���ࠢ�� � 22.10.2012*/

{globals.i}
{tmprecid.def}
{wordwrap.def}
{intrface.get xclass}


DEF VAR cnt AS INTEGER INITIAL 0 NO-UNDO.
DEF VAR sum AS DECIMAL INITIAL 0 NO-UNDO.
DEF VAR i AS INTEGER NO-UNDO.
DEF VAR str AS CHARACTER EXTENT 15 NO-UNDO.
DEF VAR tmp AS CHARACTER EXTENT 15 NO-UNDO.
DEF VAR vBranch AS CHAR NO-UNDO.
DEF VAR vINNRec AS CHAR NO-UNDO.
DEF BUFFER b-acct FOR acct.

PROCEDURE ClearTmp:
  DEF VAR ii AS INTEGER NO-UNDO.
  
  DO ii = 1 TO 15:
    tmp[ii] = "".
  END.
END PROCEDURE.

{getdate.i}

{setdest.i} 

vBranch = getThisUserXAttrValue("�⤥�����").


PUT UNFORMATTED
"��������� �� ��� ""��������������""" SKIP(1)
"������ ���⥦��� ����祭�� �� " STRING(end-date, "99/99/9999") SKIP(1)
"N ���  ��� ���.  ��� ����.    ��� ����. ���⥫�騪      �/� ����.  �㬬�        ���� �����⥫� ��� ����� �/� �����. ��� �����⥫� ��� ����� �����⥫�      �/� �����. �����祭�� ���⥦�   �� �� ���        �����       �� ���. ���.  ���.�. ��� ���.  ��� ��. ��� ��� ���⥦�" SKIP
"------ ---------- ------------ --------- --------------- ---------- ------------ --------------- --------- ---------- -------------- --------- --------------- ---------- -------------------- -- -- ---------- ----------- -- ---------- ------ ---------- ------- --- -----------" SKIP.

FOR EACH op WHERE
         op.op-date EQ     end-date AND
         op.op-kind BEGINS "klb-" NO-LOCK,
FIRST op-entry OF op
NO-LOCK,
FIRST acct WHERE 
      acct.acct      EQ op-entry.acct-db
 AND       acct.branch-id EQ vBranch
NO-LOCK,
FIRST b-acct WHERE 
      b-acct.acct    EQ op-entry.acct-cr
NO-LOCK
BREAK
 BY op-entry.amt-rub:

  cnt = cnt + 1.

  /* ����� � ��� ���㬥�� */
  str[1] = SUBSTR(op.doc-num, MAX(LENGTH(op.doc-num) - 5, 1), 6) + FILL(" ", 6 - LENGTH(op.doc-num))
         + " " + STRING(op.doc-date, "99/99/9999").
  DO i = 2 TO 15:
    str[i] = "                 ".
  END.

  FIND FIRST cust-corp WHERE cust-corp.cust-id = acct.cust-id NO-LOCK.

  /* ��� � ��� ���⥫�騪� */
  tmp[1] = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "���", "         ").
  str[1] = str[1] + " " + SUBSTR(cust-corp.inn, 1, 12) + FILL(" ", 12 - LENGTH(cust-corp.inn))
         + " " + SUBSTR(tmp[1], 1, 9) + FILL(" ", 9 - LENGTH(tmp[1])).
  DO i = 2 TO 15:
    str[i] = str[i] + "                       ".
  END.
  
  /* ������������ ���⥫�騪� */
  RUN ClearTmp.
  tmp[1] = cust-corp.cust-stat + " " + cust-corp.name-corp.
  {wordwrap.i &s=tmp &n=15 &l=15}
  DO i = 1 TO 15:
      str[i] = str[i] + " " + tmp[i] + FILL(" ", 15 - LENGTH(tmp[i])).
  END.

  /* �/� ���⥫�騪� */
  str[1] = str[1] + " " + SUBSTR(op-entry.acct-db, 1, 10).
  str[2] = str[2] + " " + SUBSTR(op-entry.acct-db, 11, 10).
  DO i = 3 TO 15:
    str[i] = str[i] + "           ".
  END.

  /* �㬬� */
  str[1] = str[1] + " " + STRING(op-entry.amt-rub, ">>>>>>>>9.99").
  DO i = 2 TO 15:
    str[i] = str[i] + "             ".
  END.
  sum = sum + op-entry.amt-rub.

  IF CAN-DO("30102*,30301*", op-entry.acct-cr) THEN DO:
    /* ���㬥�� �१ ������ ��� ��� */
    /* ���� �����⥫� */
    FIND FIRST op-bank WHERE op-bank.op = op.op AND op-bank.bank-code-type = "���-9" NO-LOCK.
    RUN ClearTmp.
    tmp[1] = op-bank.bank-name.
    {wordwrap.i &s=tmp &n=15 &l=15}
    DO i = 1 TO 15:
      str[i] = str[i] + " " + tmp[i] + FILL(" ", 15 - LENGTH(tmp[i])).
    END.
    /* ��� � �/� �����⥫� */
    IF LENGTH(op-bank.corr-acct) < 20 THEN
      tmp[1] = FILL(" ", 20).
    ELSE
      tmp[1] = op-bank.corr-acct.
    str[1] = str[1] + " " + SUBSTR(op-bank.bank-code, 1, 9) + " " + SUBSTR(tmp[1], 1, 10).
    str[2] = str[2] + "           " + SUBSTR(tmp[1], 11, 10).
    DO i = 3 TO 15:
      str[i] = str[i] + "                     ".
    END.
    /* ��� � ��� �����⥫� */
    tmp[1] = GetXAttrValueEx("op", STRING(op.op), "kpp-rec", "         ").
    vINNRec = op.inn.
    IF vINNRec EQ ? THEN
     vINNRec = "".
    str[1] = str[1] + " " + SUBSTR(vINNRec, 1, 14) + FILL(" ", 14 - LENGTH(vINNRec))
           + " " + SUBSTR(tmp[1], 1, 9) + FILL(" ", 9 - LENGTH(tmp[1])).
    DO i = 2 TO 15:
      str[i] = str[i] + "                         ".
    END.
    /* �����⥫� */
    RUN ClearTmp.
    tmp[1] = op.name-ben.
    {wordwrap.i &s=tmp &n=15 &l=15}
    DO i = 1 TO 15:
      str[i] = str[i] + " " + tmp[i] + FILL(" ", 15 - LENGTH(tmp[i])).
    END.
    /* �/� �����⥫� */
    str[1] = str[1] + " " + SUBSTR(op.ben-acct, 1, 10).
    str[2] = str[2] + " " + SUBSTR(op.ben-acct, 11, 10).
    DO i = 3 TO 15:
      str[i] = str[i] + "           ".
    END.
  END.
  ELSE DO:
    /* ���㬥�� ����७��� */
    /* ���� �����⥫� */
    RUN ClearTmp.
    tmp[1] = "��������� �� ��� ""��������������"", �.�������".
    {wordwrap.i &s=tmp &n=15 &l=15}
    DO i = 1 TO 15:
      str[i] = str[i] + " " + tmp[i] + FILL(" ", 15 - LENGTH(tmp[i])).
    END.
    /* ��� � �/� �����⥫� */
    tmp[1] = "30101810300000000923".
    str[1] = str[1] + " 040407923 " + SUBSTR(tmp[1], 1, 10).
    str[2] = str[2] + "           " + SUBSTR(tmp[1], 11, 10).
    DO i = 3 TO 15:
      str[i] = str[i] + "                     ".
    END.
    /* ���, ���, ������������ �����⥫� */    
    CASE b-acct.cust-cat:
      WHEN "�" THEN DO:
        FIND FIRST cust-corp WHERE cust-corp.cust-id = b-acct.cust-id NO-LOCK.
        tmp[1] = GetXAttrValueEx("cust-corp", STRING(cust-corp.cust-id), "���", "         ").
        str[1] = str[1] + " " + SUBSTR(cust-corp.inn, 1, 14) + FILL(" ", 14 - LENGTH(cust-corp.inn))
               + " " + SUBSTR(tmp[1], 1, 9) + FILL(" ", 9 - LENGTH(tmp[1])). 
        DO i = 2 TO 15:
          str[i] = str[i] + "                         ".
        END.
        RUN ClearTmp.
        tmp[1] = cust-corp.cust-stat + " " + cust-corp.name-corp.
        {wordwrap.i &s=tmp &n=15 &l=15}
        DO i = 1 TO 15:
          str[i] = str[i] + " " + tmp[i] + FILL(" ", 15 - LENGTH(tmp[i])).
        END.
      END.
      WHEN "�" THEN DO:
        FIND FIRST person WHERE person.person-id = b-acct.cust-id NO-LOCK.
        str[1] = str[1] + " " + SUBSTR(person.inn, 1, 14) + FILL(" ", 14 - LENGTH(person.inn)) + "          " .
        DO i = 2 TO 15:
          str[i] = str[i] + "                         ".
        END.
        RUN ClearTmp.
        tmp[1] = person.name-last + " " + person.first-names.
        {wordwrap.i &s=tmp &n=15 &l=15}
        DO i = 1 TO 15:
          str[i] = str[i] + " " + tmp[i] + FILL(" ", 15 - LENGTH(tmp[i])).
        END.
      END.
      OTHERWISE DO:
        str[1] = str[1] + " 7725114488     246643001". 
        DO i = 2 TO 15:
          str[i] = str[i] + "                         ".
        END.
        RUN ClearTmp.
        tmp[1] = "��������� �� ��� ""��������������"", �.�������".
        {wordwrap.i &s=tmp &n=15 &l=15}
        DO i = 1 TO 15:
          str[i] = str[i] + " " + tmp[i] + FILL(" ", 15 - LENGTH(tmp[i])).
        END.
      END.
    END CASE.
    /* �/� �����⥫� */
    str[1] = str[1] + " " + SUBSTR(op-entry.acct-cr, 1, 10).
    str[2] = str[2] + " " + SUBSTR(op-entry.acct-cr, 11, 10).
    DO i = 3 TO 15:
      str[i] = str[i] + "           ".
    END.
  END. 
  /* �����祭�� ���⥦� */
  RUN ClearTmp.
  tmp[1] = op.details.
  {wordwrap.i &s=tmp &n=15 &l=20}
  DO i = 1 TO 15:
    str[i] = str[i] + " " + tmp[i] + FILL(" ", 20 - LENGTH(tmp[i])).
  END.
  
  /* ��।����� � �������� ४������ */
  str[1] = str[1] + " " + SUBSTR(op.order-pay, 1, 2) + FILL(" ", 2 - LENGTH(op.order-pay))
                  + " " + SUBSTR(GetXAttrValueEx("op", STRING(op.op), "�����", "  "), 1, 2).
  DO i = 2 TO 15:
    str[i] = str[i] + "      ".
  END.
  tmp[1] = GetXAttrValueEx("op", STRING(op.op), "���", "                    ").
  str[1] = str[1] + " " + SUBSTR(tmp[1], 1, 10).
  str[2] = str[2] + " " + SUBSTR(tmp[1], 11, 10).
  DO i = 3 TO 15:
    str[i] = str[i] + "           ".
  END.
  tmp[1] = GetXAttrValueEx("op", STRING(op.op), "�����-�����", "           ").
  str[1] = str[1] + " " + SUBSTR(tmp[1], 1, 11) + FILL(" ", 11 - LENGTH(tmp[1])).
  tmp[1] = GetXAttrValueEx("op", STRING(op.op), "�����", "  ").
  str[1] = str[1] + " " + SUBSTR(tmp[1], 1, 2) + FILL(" ", 2 - LENGTH(tmp[1])).
  tmp[1] = GetXAttrValueEx("op", STRING(op.op), "�����", "          ").
  str[1] = str[1] + " " + SUBSTR(tmp[1], 1, 10) + FILL(" ", 10 - LENGTH(tmp[1])).
  tmp[1] = GetXAttrValueEx("op", STRING(op.op), "�����", "      ").
  str[1] = str[1] + " " + SUBSTR(tmp[1], 1, 6) + FILL(" ", 6 - LENGTH(tmp[1])).
  tmp[1] = GetXAttrValueEx("op", STRING(op.op), "�����", "          ").
  str[1] = str[1] + " " + SUBSTR(tmp[1], 1, 10) + FILL(" ", 10 - LENGTH(tmp[1])).
  tmp[1] = GetXAttrValueEx("op", STRING(op.op), "�����", "          ").
  str[1] = str[1] + " " + SUBSTR(tmp[1], 1, 7) + FILL(" ", 7 - LENGTH(tmp[1])).
  str[1] = str[1] + " 01  �����஭��".
  DO i = 2 TO 15:
    str[i] = str[i] + "                                                                     ".
  END.

  /* �뢮��� �� ����� */
  DO i = 1 TO 15:
    IF TRIM(str[i]) = "" THEN LEAVE.
    PUT UNFORMATTED str[i] SKIP.
  END.
END.

PUT UNFORMATTED SKIP(1) "����� " STRING(cnt) " ���㬥�⮢ �� �㬬� " STRING(sum, ">>>>>>>>>>>9.99") SKIP(1).

PUT UNFORMATTED "�ਭ�� �� ��⥬� ������-����" SKIP(1).

PUT UNFORMATTED "��� ���४⭠" SKIP.

PUT UNFORMATTED SKIP.

{preview.i}
