/*
   ��᭮��᪨� �� ��� "���ᥫ�宧����"
   ��ਫ��� �.�. 27.09.2010

   ��������� �� �����ᨨ �� ���㦨����� �।�⮢
*/

{globals.i}
{tmprecid.def}
{def_work.i new}
{sh-defs.i}
{sh-temp.i new}
{intrface.get comm}

DEF VAR h_nach AS HANDLE NO-UNDO. /* �����㬥��� ���᫥��� */

DEF VAR vDate AS DATE NO-UNDO.
DEF VAR summ_in_acct_curr LIKE acct-pos.balance NO-UNDO.
DEF VAR summ_by_ref_proc  LIKE acct-pos.balance NO-UNDO.
DEF VAR summ_in_prev_curr LIKE acct-pos.balance NO-UNDO.
DEF VAR summ_1 LIKE acct-pos.balance NO-UNDO.
DEF VAR summ_2 LIKE acct-pos.balance NO-UNDO.
DEF VAR mRate AS DEC NO-UNDO.


{getdates.i}

{setdest.i &cols=120}


RUN load_nachtool (YES, OUTPUT h_nach).  /* ����㧪� ������⥪� �奬 ���᫥��� */

in_out_ost = YES.   /*����塞 �� �室�騩 ���⮪*/

FOR EACH tmprecid,
FIRST loan WHERE RECID(loan) EQ tmprecid.id AND
      loan.cust-cat EQ "�" AND
      loan.open-date <= end-date AND
      (loan.close-date EQ ? OR loan.close-date >= beg-date) NO-LOCK,
LAST loan-acct WHERE
     loan-acct.contract  EQ "�।��"       AND
     loan-acct.cont-code EQ loan.cont-code AND
     loan-acct.acct-type EQ "�।��" AND
     loan-acct.since     <= end-date NO-LOCK,
LAST acct where acct.acct =  loan-acct.acct NO-LOCK
BREAK BY loan-acct.acct:

   vDate = (IF acct.open-date >= beg-date THEN acct.open-date + 1 ELSE beg-date).
   /*���� %%*/
   RUN RUN_NACH in h_nach (vDate,
                           end-date,
                           RECID(acct),
                           acct.currency,
                           ?,
                           "filtopen",
                           "%����।��",
                           output summ_in_acct_curr,
                           output summ_by_ref_proc,
                           output summ_in_prev_curr).

  /* �⠢��  */
  mRate = GET_COMM ("%�����",
                       ?,
                       loan.currency,
                       loan.contract + "," + loan.cont-code,
                       0.0,
                       0,
                       end-date).

   FOR EACH nach_rep
   NO-LOCK
   BREAK
    BY nach_rep.acct:

     /*��砫� ���ᠭ�� ���*/
     FORM nach_rep.acct         FORMAT "x(20)" COLUMN-LABEL "����"
          loan-acct.cont-code   FORMAT "x(18)" COLUMN-LABEL "�������"
          nach_rep.intrvl_beg   COLUMN-LABEL "�"
          nach_rep.intrvl_end   COLUMN-LABEL "��"
          nach_rep.day_p_int    COLUMN-LABEL "���-��!����"
          nach_rep.match_rem    FORMAT ">,>>>,>>>,>>>,>>9.99" COLUMN-LABEL "�������"
          mRate                 format ">9.99999" COLUMN-LABEL "������"
          nach_rep.acct_val_per COLUMN-LABEL "���������"
          summ_1                COLUMN-LABEL "���������!��� ���"
          summ_2                COLUMN-LABEL "���"

/*     HEADER  "��������� �� �������� �� ������������ ��������" FORMAT "x(70)" SKIP
             CAPS({term2str Beg-Date End-Date}) FORMAT "x(70)" SKIP(2)*/
     WITH NO-BOX WIDTH 200.
     /*����� ���ᠭ�� ���*/

     DISPLAY nach_rep.acct WHEN FIRST-OF (nach_rep.acct).
     DISPLAY loan-acct.cont-code WHEN FIRST-OF (nach_rep.acct).
     DISPLAY nach_rep.intrvl_beg.
     DISPLAY nach_rep.intrvl_end.
     DISPLAY nach_rep.day_p_int.
     DISPLAY nach_rep.match_rem.
     DISPLAY mRate.
     DISPLAY nach_rep.acct_val_per.
     IF LAST-OF (nach_rep.acct) THEN
     DO:
      DOWN.
      DISPLAY "�⮣� �� ����:" @ nach_rep.acct.
      DISPLAY summ_in_acct_curr @ nach_rep.acct_val_per.
      DISPLAY (summ_in_acct_curr / 1.18) @ summ_1.
      DISPLAY (summ_in_acct_curr - summ_in_acct_curr / 1.18) @ summ_2.
      DOWN(1).
     END.
   END.
END. 

RUN remove_nachtool (YES, h_nach).
   
{signatur.i &user-only = YES}

{intrface.del}

{preview.i}
