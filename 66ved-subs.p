/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2007 ЗАО "Банковские информационные системы"
     Filename: ved-subs.p
      Comment: Отчетные формы по расчету размера субсидий
   Parameters:
         Uses:
      Used by:
      Created: 22.01.2007 13:08 Fepa 69787
*/

DEF INPUT PARAM iNumFrmSel AS CHAR NO-UNDO. /* Входной параметр - номер формы */

{globals.i}             /* Глобальные переменные сессии. */
{svarloan.def}          /* Переменные модуля кредиты и депозиты. */
{tmprecid.def}          /* Таблица с выбранными записями. */
{t-otch.i new}          /* Таблица отчета по одному договору. */
{66ved-subs.def}          /* Временные таблицы для печати */
{intrface.get xclass}   /* Подключени инструменетов метасхемы. */
{intrface.get instrum}  /* Подключени инструменетов перевода валют. */
{intrface.get cust}
{intrface.get loan}
{intrface.get comm}
{intrface.get tmess}    /* Инструменты обработки сообщений. */
{loan.pro}
{wordwrap.def}

DEF VAR mBegDate       AS DATE NO-UNDO. /* Дата начала периода. */
DEF VAR mEndDate       AS DATE NO-UNDO. /* Дата окончания периода. */
DEF VAR mListParam     AS CHAR NO-UNDO. /* Список параметров для вычисления просрочки в CHeckLoan */
DEF VAR mClientName    AS CHAR NO-UNDO. /* Наименование клиента */
DEF VAR rat-cred       AS DEC  NO-UNDO. /* Процентная ставка */
DEF VAR rat-ref        AS DEC  NO-UNDO. /* Ставка рефинансирования */
DEF VAR mAcct          AS CHAR NO-UNDO. /* Счет заемщика для перечисления субсидий */
DEF VAR mDaysYear      AS INT64  NO-UNDO. /* Количество дней в году */



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
DEF VAR mOpBegDate     AS DATE NO-UNDO. /* (Вх.) Начальная дата, на которую строится отчет. */
DEF VAR mOpEndDate     AS DATE NO-UNDO. /* (Вх.) Конечная дата, на которую строится отчет. */
DEF VAR mSubsRate      AS INT64  NO-UNDO. /* (Вх.) Размер федеральной субсидии (%) */
DEF VAR mSubsRateRg    AS INT64  NO-UNDO. /* (Вх.) Размер региональной субсидии (%) */
DEF VAR mValidInPar    AS CHAR NO-UNDO. /* список корректных входных параметров */
DEF VAR mBankBIK       AS CHAR NO-UNDO. /* БИК банка */
DEF VAR mCorrAcct      AS CHAR NO-UNDO. /* Корр. счет банка */
DEF VAR mIsCurVed      AS LOG  NO-UNDO. 
DEF VAR mBankRuk       AS CHAR NO-UNDO. /* ФИО руководителя банка */
DEF VAR mBankBuch      AS CHAR NO-UNDO. /* ФИО глав.буха банка */
DEF VAR mdatasogl      AS DATE NO-UNDO. /* дата заключения кред. договора (ДР ДатаСогл) */
DEF VAR vSince         AS DATE NO-UNDO. /* Дата пересчета договора */
DEF VAR mADelay        AS LOG  NO-UNDO. /* Учет просрочки */

DEF VAR vAdress AS CHAR NO-UNDO. /* это        */
DEF VAR vINN    AS CHAR NO-UNDO. /* все        */
DEF VAR vKPP    AS CHAR NO-UNDO. /* для        */
DEF VAR vType   AS CHAR NO-UNDO. /* нормальной */
DEF VAR vCode   AS CHAR NO-UNDO. /* работы     */
DEF VAR vAcct   AS CHAR NO-UNDO. /* getcliname */

def var adr_rt  AS CHAR NO-UNDO.

{ved-subs.pro}
{66ved-subs.frm}
{ved-inp.frm}
{66ved-subs.prn}

ASSIGN
   mListParam  = "7,10,13,16,34,48,248"
   mValidInPar = "1,2"
   mADelay     = FGetSetting("УчПрос",?,"НЕТ") EQ "ДА"
.


MAIN_BLOCK:
DO
ON ERROR  UNDO MAIN_BLOCK, LEAVE MAIN_BLOCK
ON ENDKEY UNDO MAIN_BLOCK, LEAVE MAIN_BLOCK:
   /* Проверка номера формы
   ** Если не указана, то запрашиваем */
   IF iNumFrmSel EQ "" THEN
      d:   
      DO ON ENDKEY UNDO, LEAVE:
         PAUSE 0.
         SET iNumFrmSel WITH FRAME FormNum.
         HIDE FRAME FormNum.
         IF NOT CAN-DO(mValidInPar,iNumFrmSel) THEN DO:
            RUN Fill-SysMes IN h_tmess ("", "", "0", "Неверный код формы. Введите 1 или 2.").
            UNDO d, RETRY d.
         END.
      END.
   IF iNumFrmSel EQ "" THEN LEAVE MAIN_BLOCK.
   
   /* Запрос параметров */
   GET_PAR:
   DO
   ON ENDKEY UNDO GET_PAR, LEAVE GET_PAR
   WITH FRAME fGetPar:
      /* начальные значения */
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
   IF LAST-EVENT:FUNCTION EQ "END-ERROR" THEN LEAVE MAIN_BLOCK.   /* нажата Ecs - выход */
   
   ASSIGN
      name-bank   = FGetSetting("Банк",?,"")
      mBankBIK    = FGetSetting("БанкМФО",?,"")
      mCorrAcct   = FGetSetting("КорСч",?,"")
      mBankRuk    = FGetSetting("ФИОРук",?,"")  /* ФИО руководителя банка */
      mBankBuch   = FGetSetting("ФИОБух",?,"")  /* ФИО глав.буха банка */
      .

   /* Т.к. удовлетворяющий требованиям пересчет договора это 
   ** l-calc2.p, а в нем чистится tmprecid, то копируем во 
   ** временную табличку и работаем с ней */
   FOR EACH tmprecid:
      CREATE tmprecs.
      BUFFER-COPY tmprecid TO tmprecs.
   END.

   /* Перебираем все отмеченные записи */
   NEXT_LOAN:
   FOR EACH tmprecs, 
   FIRST loan WHERE
      RECID(loan) EQ tmprecs.id 
   NO-LOCK:
      /* Проверка - надо ли печатать инфу по данному договору */
      mChOk = CheckLoan(BUFFER loan).
      IF NOT mChOk THEN NEXT NEXT_LOAN.

         /* Определяем требуемую дату пересчета договора */
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
         /* Если неодходимо, персчитываем договор вперед, 
         ** иначе неверно определяется задолженность */
      IF loan.since LT vSince THEN
         RUN "l-calc2.p" (loan.contract, 
                          loan.cont-code, 
                          vSince, 
                          YES, 
                          YES).
         
      /* Наименование клиента */
      mClientName = GetCliName( loan.cust-cat,
                                STRING(loan.cust-id),
                                OUTPUT vAdress,
                                OUTPUT vINN,
                                OUTPUT vKPP,
                                INPUT-OUTPUT vType,
                                OUTPUT vCode,       
                                OUTPUT vAcct
                               ).

      /* Счет заемщика для перечисления субсидий */
      RUN RE_L_ACCT IN h_Loan (loan.contract,
                               loan.cont-code,
                               "КредРасч",
                               mOpEndDate,
                               BUFFER loan-acct).

      mAcct = IF AVAIL loan-acct 
                       THEN loan-acct.acct 
                       ELSE "".

      /* Определение ставки рефинансирования (на дату заключения кред. договора) */
      mdatasogl = DATE (GetXAttrValueEx("loan",loan.contract + "," + loan.cont-code,"ДатаСогл","?")).
      IF mdatasogl EQ ? THEN mdatasogl = loan.open-date.
      rat-ref = GET_COMM("%ЦБРеф",                             /* Код комиссии */
                         ?,                                    /* recid счета*/
                         loan.currency,                        /* валюта*/
                         "",                                   /* Кау*/
                         0.00,                                 /* MIN остаток*/
                         0,                                    /* период */
                         mdatasogl).                           /* дата */

      /* Формирование информации по договору. */
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
         RepInfo.datenum    = GetXattrValueEx("loan",loan.contract + "," + loan.cont-code,"ДатаСогл",STRING(loan.open-date))  + ";" + loan.doc-ref
         RepInfo.rat-ref    = rat-ref
            /* %% ставки иногда меняются, выносим в LoanInfo.rat-cred (а rat-ref может меняться ?)*/
         RepInfo.rat-cred   = 0           
      .

      /* обязательства по процентам договора в заданном периоде */
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
         /* поиск даты начала и окончания текущего процентного периода */
         RUN GetDateBegEnd (BUFFER loan, term-obl.end-date - 1, OUTPUT mBegDate, OUTPUT mEndDate).
         /* дата начала текущего периода оказалась меньше даты начала расчета ведомости */
         IF     NOT mADelay 
            AND mBegDate LT mOpBegDate THEN NEXT TERM_OBL.
         /* закончились платежные периоды */
         IF    mEndDate GT mOpEndDate
            OR mEndDate EQ ?
         THEN LEAVE TERM_OBL.

         /* нужно ли формировать ведомость по процентному периоду */
         mChOk = CheckTermObl(BUFFER loan, mBegDate, mEndDate, mOpEndDate).
         IF NOT mChOk THEN NEXT TERM_OBL.

         /* Получение информации по одному договору. */
         RUN pint.p (loan.contract, loan.cont-code, mBegDate, mEndDate, "*").
      
         RUN DeleteOldDataProtocol IN h_base("СводныйГрафикОстатков").
         RUN DeleteOldDataProtocol IN h_base("СводныйГрафикКомиссий").
         RUN DeleteOldDataProtocol IN h_base("ДатаОстатка").
         RUN DeleteOldDataProtocol IN h_base("СуммаОстатка").
         RUN DeleteOldDataProtocol IN h_base("ПересчетППАннуитета").
         RUN DeleteOldDataProtocol IN h_base("ПерваяИтерацияПересчета").
         RUN DeleteOldDataProtocol IN h_base("ДатаОплаты%").
         RUN DeleteOldDataProtocol IN h_base("РасчетЗадолженностиПоСрокам").
      
         /* Проверка наличия информации по договору. */
         IF NOT CAN-FIND (FIRST otch1)
            THEN NEXT TERM_OBL.

         /* Формирование сводного отчета. */
         /* Получение информации по периодам начисления. */
         OtchRecord:
         FOR EACH otch1:
            /* Вычисление дней в году - если делится без 
            ** остатка на 4, то высокосный, иначе нет 
            ** т.к. otch1 корректно ставит разбиения по вис.годам,
            ** то ориентируемся в расчете кол-ва дней на otch1 */
            mDaysYear = IF YEAR(otch1.end-date) MODULO 4 NE 0 
                           THEN 365
                           ELSE 366.
               /* Процентная ставка на конец периода */                    /* Дата */
            FIND LAST comm-rate WHERE comm-rate.commission EQ "%Кред"
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
      /* нумеруем только отчеты по договорам с информацией по периодам,
      ** чтобы не возникало разрывов в нумерации */
      IF CAN-FIND(FIRST LoanInfo WHERE LoanInfo.cont-code EQ RepInfo.cont-code) THEN
         ASSIGN
            mNumber2           = mNumber2 + 1
            RepInfo.nn         = mNumber2
          .
      NEXT NEXT_LOAN.
   END. /* for each tmprecs */

   FOR EACH repinfo:
       FOR EACH loaninfo WHERE LoanInfo.cont-code EQ RepInfo.cont-code 
                           AND LoanInfo.comment   EQ "Основные проценты":
          ASSIGN
            repinfo.itog_summ_opl = repinfo.itog_summ_opl + LoanInfo.summ_pr.
       END.
       ASSIGN
          vTotal9     = vTotal9     + repinfo.itog_summ_opl.
   END.
   
   /* Протокол об ошибках */
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
   
   /* Проверка наличия отчета. */
   IF NOT CAN-FIND (FIRST RepInfo)
   THEN DO:
      RUN Fill-SysMes IN h_tmess ("", "", "0", "Отчет пуст!").
      LEAVE MAIN_BLOCK.
   END.
   
   /* Печать по форме */
   IF CAN-DO(mValidInPar, iNumFrmSel) THEN 
   DO:
      {setdest.i}
      RUN VALUE ("PrintForm" + iNumFrmSel).
      {signatur.i}
      {preview.i}
   END.
   ELSE
      RUN Fill-SysMes IN h_tmess ("", "", "0", "Форма с номером '" + iNumFrmSel + "' не найдена!").

  /* Экспорт в DBF */
  SUBSCRIBE TO "ESUBS-DATA-REQ" ANYWHERE RUN-PROCEDURE "SetSubsTT".
  FIND FIRST op-kind WHERE op-kind.op-kind EQ "_e-subs" NO-LOCK NO-ERROR.
  IF AVAIL op-kind THEN
  DO:
     RUN VALUE (op-kind.proc + ".p") (TODAY, RECID (op-kind)).
  END.
  
  UNSUBSCRIBE TO "ESUBS-DATA-REQ".
END.  /* of MAIN_BLOCK */

{intrface.del}          /* Выгрузка инструментария. */
RETURN.

PROCEDURE SetSubsTT.
   DEFINE OUTPUT PARAMETER oRepInfoHdl  AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oLoanInfoHdl AS CHARACTER   NO-UNDO.

   oRepInfoHdl  = STRING (TEMP-TABLE RepInfo:HANDLE).
   oLoanInfoHdl = STRING (TEMP-TABLE LoanInfo:HANDLE).

   RETURN "".
END PROCEDURE.
