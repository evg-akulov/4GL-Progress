/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2007 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: ved-subs.p
      Comment: ����� ��� �� ����� ࠧ��� ��ᨤ��
   Parameters:
         Uses:
      Used by:
      Created: 22.01.2007 13:08 Fepa 69787
*/

DEF INPUT PARAM iNumFrmSel AS CHAR NO-UNDO. /* �室��� ��ࠬ��� - ����� ��� */

{globals.i}             /* �������� ��६���� ��ᨨ. */
{svarloan.def}          /* ��६���� ����� �।��� � ��������. */
{tmprecid.def}          /* ������ � ��࠭�묨 �����ﬨ. */
{t-otch.i new}          /* ������ ���� �� ������ ��������. */
{66ved-subs.def}          /* �६���� ⠡���� ��� ���� */
{intrface.get xclass}   /* ������祭� �����㬥��⮢ ����奬�. */
{intrface.get instrum}  /* ������祭� �����㬥��⮢ ��ॢ��� �����. */
{intrface.get cust}
{intrface.get loan}
{intrface.get comm}
{intrface.get tmess}    /* �����㬥��� ��ࠡ�⪨ ᮮ�饭��. */
{loan.pro}
{wordwrap.def}

DEF VAR mBegDate       AS DATE NO-UNDO. /* ��� ��砫� ��ਮ��. */
DEF VAR mEndDate       AS DATE NO-UNDO. /* ��� ����砭�� ��ਮ��. */
DEF VAR mListParam     AS CHAR NO-UNDO. /* ���᮪ ��ࠬ��஢ ��� ���᫥��� ����窨 � CHeckLoan */
DEF VAR mClientName    AS CHAR NO-UNDO. /* ������������ ������ */
DEF VAR rat-cred       AS DEC  NO-UNDO. /* ��業⭠� �⠢�� */
DEF VAR rat-ref        AS DEC  NO-UNDO. /* �⠢�� �䨭���஢���� */
DEF VAR mAcct          AS CHAR NO-UNDO. /* ��� ����騪� ��� ����᫥��� ��ᨤ�� */
DEF VAR mDaysYear      AS INT64  NO-UNDO. /* ������⢮ ���� � ���� */



DEF VAR mchok          AS LOG  NO-UNDO.
DEF VAR mNumber        AS INT64  NO-UNDO.
DEF VAR mNumber2       AS INT64  NO-UNDO.
DEF VAR name-bank      AS CHAR NO-UNDO.
DEF VAR vTotal9        AS DEC  NO-UNDO.
DEF VAR vTotal13       AS DEC  NO-UNDO.
DEF VAR mTotalSubs1    AS DEC  NO-UNDO.
DEF VAR mTotalSubs2    AS DEC  NO-UNDO.
DEF VAR mTotalSubs3    AS DEC  NO-UNDO.

DEF VAR vMes           AS CHAR EXTENT 5 NO-UNDO.
DEF VAR vMesDN         AS CHAR EXTENT 5 NO-UNDO.
DEF VAR i              AS INT64  NO-UNDO.
DEF VAR vKolvo         AS INT64  NO-UNDO.
DEF VAR vKolvoDn       AS INT64  NO-UNDO.
DEF VAR vStrNum        AS INT64  NO-UNDO.
DEF VAR mOpBegDate     AS DATE NO-UNDO. /* (��.) ��砫쭠� ���, �� ������ ��ந��� ����. */
DEF VAR mOpEndDate     AS DATE NO-UNDO. /* (��.) ����筠� ���, �� ������ ��ந��� ����. */
DEF VAR mSubsRate      AS INT64  NO-UNDO. /* (��.) ������ 䥤�ࠫ쭮� ��ᨤ�� (%) */
DEF VAR mSubsRateRg    AS INT64  NO-UNDO. /* (��.) ������ ॣ�����쭮� ��ᨤ�� (%) */
DEF VAR mValidInPar    AS CHAR NO-UNDO. /* ᯨ᮪ ���४��� �室��� ��ࠬ��஢ */
DEF VAR mBankBIK       AS CHAR NO-UNDO. /* ��� ����� */
DEF VAR mCorrAcct      AS CHAR NO-UNDO. /* ����. ��� ����� */
DEF VAR mIsCurVed      AS LOG  NO-UNDO. 
DEF VAR mBankRuk       AS CHAR NO-UNDO. /* ��� �㪮����⥫� ����� */
DEF VAR mBankBuch      AS CHAR NO-UNDO. /* ��� ����.��� ����� */
DEF VAR mdatasogl      AS DATE NO-UNDO. /* ��� �����祭�� �।. ������� (�� ��⠑���) */
DEF VAR vSince         AS DATE NO-UNDO. /* ��� ������ ������� */
DEF VAR mADelay        AS LOG  NO-UNDO. /* ��� ����窨 */

DEF VAR vAdress AS CHAR NO-UNDO. /* ��        */
DEF VAR vINN    AS CHAR NO-UNDO. /* ��        */
DEF VAR vKPP    AS CHAR NO-UNDO. /* ���        */
DEF VAR vType   AS CHAR NO-UNDO. /* ��ଠ�쭮� */
DEF VAR vCode   AS CHAR NO-UNDO. /* ࠡ���     */
DEF VAR vAcct   AS CHAR NO-UNDO. /* getcliname */

def var adr_rt  AS CHAR NO-UNDO.

{ved-subs.pro}
{66ved-subs.frm}
{ved-inp.frm}
{66ved-subs.prn}

ASSIGN
   mListParam  = "7,10,13,16,34,48,248"
   mValidInPar = "1,2"
   mADelay     = FGetSetting("����",?,"���") EQ "��"
.


MAIN_BLOCK:
DO
ON ERROR  UNDO MAIN_BLOCK, LEAVE MAIN_BLOCK
ON ENDKEY UNDO MAIN_BLOCK, LEAVE MAIN_BLOCK:
   /* �஢�ઠ ����� ���
   ** �᫨ �� 㪠����, � ����訢��� */
   IF iNumFrmSel EQ "" THEN
      d:   
      DO ON ENDKEY UNDO, LEAVE:
         PAUSE 0.
         SET iNumFrmSel WITH FRAME FormNum.
         HIDE FRAME FormNum.
         IF NOT CAN-DO(mValidInPar,iNumFrmSel) THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "0", "������ ��� ���. ������ 1 ��� 2.").
            UNDO d, RETRY d.
         END.
      END.
   IF iNumFrmSel EQ "" THEN LEAVE MAIN_BLOCK.
   
   /* ����� ��ࠬ��஢ */
   GET_PAR:
   DO
   ON ENDKEY UNDO GET_PAR, LEAVE GET_PAR
   WITH FRAME fGetPar:
      /* ��砫�� ���祭�� */
      ASSIGN mOpEndDate  = gend-date
             mSubsRate   = 95
             mSubsRateRg = 0.
      PAUSE 0.
      IF mADelay THEN
      DO:
         UPDATE
            mOpEndDate
            mSubsRate
            mSubsRateRg.
      END.
      ELSE
      DO:
         UPDATE
            mOpBegDate
            mOpEndDate
            mSubsRate
            mSubsRateRg.
      END.
   END.
   HIDE FRAME fGetPar NO-PAUSE.
   IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.   /* ����� Ecs - ��室 */
   
   ASSIGN
      name-bank   = FGetSetting("����",?,"")
      mBankBIK    = FGetSetting("�������",?,"")
      mCorrAcct   = FGetSetting("�����",?,"")
      mBankRuk    = FGetSetting("�����",?,"")  /* ��� �㪮����⥫� ����� */
      mBankBuch   = FGetSetting("������",?,"")  /* ��� ����.��� ����� */
      .

   /* �.�. 㤮���⢮���騩 �ॡ������ ������ ������� �� 
   ** l-calc2.p, � � ��� ������ tmprecid, � �����㥬 �� 
   ** �६����� ⠡���� � ࠡ�⠥� � ��� */
   FOR EACH tmprecid:
      CREATE tmprecs.
      BUFFER-COPY tmprecid TO tmprecs.
   END.

   /* ��ॡ�ࠥ� �� �⬥祭�� ����� */
   NEXT_LOAN:
   FOR EACH tmprecs, 
   FIRST loan WHERE
      RECID(loan) EQ tmprecs.id 
   NO-LOCK:
      /* �஢�ઠ - ���� �� ������ ���� �� ������� �������� */
      mChOk = CheckLoan(BUFFER loan).
      IF NOT mChOk THEN NEXT NEXT_LOAN.

         /* ��।��塞 �ॡ㥬�� ���� ������ ������� */
      vSince = loan.since.
      RUN RE_TERM_OBL (loan.contract, 
                       loan.cont-code, 
                       3, 
                       mOpEndDate, 
                       BUFFER term-obl).
      IF AVAIL term-obl THEN 
         vSince = MAX (vSince, 
                      (IF term-obl.dsc-beg-date NE ? 
                          THEN term-obl.dsc-beg-date 
                          ELSE term-obl.end-date) + 1).
         /* �᫨ ����室���, ������뢠�� ������� ���।, 
         ** ���� ����୮ ��।������ ������������� */
      IF loan.since LT vSince THEN
         RUN "l-calc2.p" (loan.contract, 
                          loan.cont-code, 
                          vSince, 
                          YES, 
                          YES).
         
      /* ������������ ������ */
      mClientName = GetCliName( loan.cust-cat,
                                STRING(loan.cust-id),
                                OUTPUT vAdress,
                                OUTPUT vINN,
                                OUTPUT vKPP,
                                INPUT-OUTPUT vType,
                                OUTPUT vCode,       
                                OUTPUT vAcct
                               ).

      /* ��� ����騪� ��� ����᫥��� ��ᨤ�� */
      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               "�।����",
                               mOpEndDate,
                               BUFFER loan-acct).

      mAcct = IF AVAIL loan-acct 
                       THEN loan-acct.acct 
                       ELSE "".

      /* ��।������ �⠢�� �䨭���஢���� (�� ���� �����祭�� �।. �������) */
      mdatasogl = DATE (GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"��⠑���","?")).
      IF mdatasogl EQ ? THEN mdatasogl = loan.open-date.
      rat-ref = GET_COMM("%�����",                             /* ��� �����ᨨ */
                         ?,                                    /* recid ���*/
                         loan.currency,                        /* �����*/
                         "",                                   /* ���*/
                         0.00,                                 /* MIN ���⮪*/
                         0,                                    /* ��ਮ� */
                         mdatasogl).                           /* ��� */

      /* ��ନ஢���� ���ଠ樨 �� ��������. */
      CREATE RepInfo.
      ASSIGN
         RepInfo.cont-code  = loan.cont-code
         RepInfo.doc-num    = loan.doc-ref
         RepInfo.currency   = loan.currency
         RepInfo.cust-cat   = loan.cust-cat
         RepInfo.cust-id    = loan.cust-id
         RepInfo.ClientName = mClientName + ";" + vINN
         RepInfo.open-date  = loan.open-date
         RepInfo.acct       = mAcct
         RepInfo.datenum    = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"��⠑���",STRING(loan.open-date))  + ";" + loan.doc-ref
         RepInfo.rat-ref    = rat-ref
            /* %% �⠢�� ������ ��������, �뭮ᨬ � LoanInfo.rat-cred (� rat-ref ����� �������� ?)*/
         RepInfo.rat-cred   = 0           
      .

      /* ��易⥫��⢠ �� ��業⠬ ������� � �������� ��ਮ�� */
      TERM_OBL:
      FOR EACH term-obl WHERE 
                   (    mADelay
                    AND term-obl.contract  EQ loan.contract
                    AND term-obl.cont-code EQ loan.cont-code
                    AND term-obl.end-date  LE mOpEndDate
                    AND term-obl.idnt      EQ 1)
                OR 
                   (NOT mADelay
                    AND term-obl.contract  EQ loan.contract
                    AND term-obl.cont-code EQ loan.cont-code
                    AND term-obl.end-date  GE mOpBegDate
                    AND term-obl.end-date  LE mOpEndDate
                    AND term-obl.idnt      EQ 1)
      NO-LOCK:
         /* ���� ���� ��砫� � ����砭�� ⥪�饣� ��業⭮�� ��ਮ�� */
         RUN GetDateBegEnd (BUFFER loan, term-obl.end-date - 1, OUTPUT mBegDate, OUTPUT mEndDate).
         /* ��� ��砫� ⥪�饣� ��ਮ�� ��������� ����� ���� ��砫� ���� �������� */
         IF     NOT mADelay 
            AND mBegDate LT mOpBegDate THEN NEXT TERM_OBL.
         /* �����稫��� ���⥦�� ��ਮ�� */
         IF    mEndDate GT mOpEndDate
            OR mEndDate EQ ?
         THEN LEAVE TERM_OBL.

         /* �㦭� �� �ନ஢��� ��������� �� ��業⭮�� ��ਮ�� */
         mChOk = CheckTermObl(BUFFER loan, mBegDate, mEndDate, mOpEndDate).
         IF NOT mChOk THEN NEXT TERM_OBL.

         /* ����祭�� ���ଠ樨 �� ������ ��������. */
         RUN pint.p (loan.contract, loan.cont-code, mBegDate, mEndDate, "*").
      
         RUN DeleteOldDataProtocol IN h_base("�����멃�䨪���⪮�").
         RUN DeleteOldDataProtocol IN h_base("�����멃�䨪�����ᨩ").
         RUN DeleteOldDataProtocol IN h_base("��⠎��⪠").
         RUN DeleteOldDataProtocol IN h_base("�㬬����⪠").
         RUN DeleteOldDataProtocol IN h_base("�����⏏������").
         RUN DeleteOldDataProtocol IN h_base("��ࢠ��������").
         RUN DeleteOldDataProtocol IN h_base("��⠎�����%").
         RUN DeleteOldDataProtocol IN h_base("����⇠���������⨏��ப��").
      
         /* �஢�ઠ ������ ���ଠ樨 �� ��������. */
         IF NOT CAN-FIND (FIRST otch1)
            THEN NEXT TERM_OBL.

         /* ��ନ஢���� ᢮����� ����. */
         /* ����祭�� ���ଠ樨 �� ��ਮ��� ���᫥���. */
         OtchRecord:
         FOR EACH otch1:
            /* ���᫥��� ���� � ���� - �᫨ ������� ��� 
            ** ���⪠ �� 4, � ��᮪���, ���� ��� 
            ** �.�. otch1 ���४⭮ �⠢�� ࠧ������ �� ���.�����,
            ** � �ਥ���㥬�� � ���� ���-�� ���� �� otch1 */
            mDaysYear = IF YEAR(otch1.end-date) MODULO 4 NE 0 
                           THEN 365
                           ELSE 366.
               /* ��業⭠� �⠢�� �� ����� ��ਮ�� */                    /* ��� */
            FIND LAST comm-rate WHERE comm-rate.commission EQ "%�।"
                                  AND comm-rate.kau        EQ loan.contract + "," + loan.cont-code
                                  AND comm-rate.since      LE otch1.end-date
            NO-LOCK NO-ERROR.
            ASSIGN
               rat-cred = IF AVAIL comm-rate THEN comm-rate.rate-comm ELSE 0
            .
            CREATE LoanInfo.
            ASSIGN
               LoanInfo.cont-code = loan.cont-code
               LoanInfo.bal-summ  = otch1.bal-summ
               LoanInfo.beg-date  = otch1.beg-date
               LoanInfo.end-date  = otch1.end-date
               LoanInfo.ndays     = otch1.ndays
               LoanInfo.rat-cred  = rat-cred
               LoanInfo.rat1      = otch1.summ_pr
               LoanInfo.summ_pr   = otch1.summ_pr
               LoanInfo.comment   = otch1.comment
	       LoanInfo.Raion     = trim(substring(vAdress,8,15))
            .

         END.  /* FOR EACH term-obl */
      END. /* for each otch1 */
      /* �㬥�㥬 ⮫쪮 ����� �� ������ࠬ � ���ଠ樥� �� ��ਮ���,
      ** �⮡� �� ��������� ࠧ�뢮� � �㬥�樨 */
      IF CAN-FIND(FIRST LoanInfo WHERE LoanInfo.cont-code EQ RepInfo.cont-code) THEN
         ASSIGN
            mNumber2           = mNumber2 + 1
            RepInfo.nn         = mNumber2
          .
      NEXT NEXT_LOAN.
   END. /* for each tmprecs */

   FOR EACH repinfo:
       FOR EACH loaninfo WHERE LoanInfo.cont-code EQ RepInfo.cont-code 
                           AND LoanInfo.comment   EQ "�᭮��� ��業��":
          ASSIGN
            repinfo.itog_summ_opl = repinfo.itog_summ_opl + LoanInfo.summ_pr.
       END.
       ASSIGN
          vTotal9     = vTotal9     + repinfo.itog_summ_opl.
   END.
   
   /* ��⮪�� �� �訡��� */
   IF CAN-FIND(FIRST errinfo) THEN 
   DO:
      {setdest.i &filename='err_vedsubs.log'}
      RUN PrintError.
      {preview.i &filename='err_vedsubs.log'}
   END.
   
   FOR EACH RepInfo:
       IF NOT CAN-FIND(FIRST LoanInfo WHERE LoanInfo.cont-code EQ RepInfo.cont-code) THEN
       DELETE RepInfo.
   END.
   
   /* �஢�ઠ ������ ����. */
   IF NOT CAN-FIND (FIRST RepInfo)
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "0", "���� ����!").
      LEAVE MAIN_BLOCK.
   END.
   
   /* ����� �� �ଥ */
   IF CAN-DO(mValidInPar, iNumFrmSel) THEN 
   DO:
      {setdest.i}
      RUN VALUE ("PrintForm" + iNumFrmSel).
      {signatur.i}
      {preview.i}
   END.
   ELSE
      RUN Fill-SysMes IN h_tmess ("", "", "0", "��ଠ � ����஬ '" + iNumFrmSel + "' �� �������!").

  /* ��ᯮ�� � DBF */
  SUBSCRIBE TO "ESUBS-DATA-REQ" ANYWHERE RUN-PROCEDURE "SetSubsTT".
  FIND FIRST op-kind WHERE op-kind.op-kind EQ "_e-subs" NO-LOCK NO-ERROR.
  IF AVAIL op-kind THEN
  DO:
     RUN VALUE (op-kind.proc + ".p") (TODAY, RECID (op-kind)).
  END.
  
  UNSUBSCRIBE TO "ESUBS-DATA-REQ".
END.  /* of MAIN_BLOCK */

{intrface.del}          /* ���㧪� �����㬥����. */
RETURN.

PROCEDURE SetSubsTT.
   DEFINE OUTPUT PARAMETER oRepInfoHdl  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oLoanInfoHdl AS CHARACTER   NO-UNDO.

   oRepInfoHdl  = STRING (TEMP-TABLE RepInfo:HANDLE).
   oLoanInfoHdl = STRING (TEMP-TABLE LoanInfo:HANDLE).

   RETURN "".
END PROCEDURE.
