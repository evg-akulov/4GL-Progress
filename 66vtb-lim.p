/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ТОО "Банковские информационные системы"
     Filename: vtb-lim.p
      Comment: Состояние лимита, ссудная задолженность по состоянию на .... 
   Parameters: 
         Uses:                     
      Used by:                     
      Created:
     Modified: 08.08.2003 15:17 KSV      (0010770) Исправлена орфографическая
                                         ошибка в заголовке отчета. Исправлена
                                         ошибка задвоения итога по лимиту
                                         кредитования.
     Modified: 04.03.2011 16:46 krag     <comment>
*/

{globals.i}
{getdate.i}
end-date = end-date - 1.
{intrface.get comm}
{intrface.get xclass}
{intrface.get loan}
{intrface.get instrum}

def var c     as INT64 initial 0  no-undo.
def var name  as char               no-undo.
def var name1 as char    extent  3  no-undo.
def var cmm   as decimal  no-undo.
def var ds    as date     no-undo.
def var r-bad as decimal  no-undo.
def var r$    as decimal  no-undo.
def var db-tmp as decimal     no-undo.
def var cr-tmp as decimal     no-undo.
def var s-limit$ as decimal no-undo.
def var s-limit  as decimal no-undo.
DEF VAR teleph       AS CHAR NO-UNDO.

{tmprecid.def}

{strtout3.i &col=150}
put unformatted "Состояние лимита, ссудная задолженность по состоянию на " 
   (end-date + 1) skip(2).


for each tmprecid,
    each loan where recid(loan) = tmprecid.id
                and loan.close-date eq ?
                no-lock
    break by loan.cont-code :
  c = c + 1. display c format ">>>9" column-label "П/П" .

  /* Заемщик */
  run GetCustName IN h_base(loan.cust-cat,loan.cust-id,?,
                            output name1[1],
                            output name1[2],
                            input-output name1[3]).
  name = trim(name1[1] + " " + name1[2]).
  display name format "x(35)" column-label "Заемщик" with scrollable .

  /*телефон*/
   teleph = "".
/*   find first person where person.person-id = loan.cust-id.
   IF AVAIL person THEN assign teleph = trim(person.phone[1]) + " " + trim(person.phone[2]).
   display teleph format "x(38)" column-label "телефон".*/

/*  display  getUserXAttrValue(loan.user-id, "Отделение") format "x(4)" column-label "ДО".*/
  display loan.branch-id format "x(4)" column-label "ДО".

  find first _user 
	where _user._Userid = loan.user-id.
  display  _user._User-Name format "x(30)" column-label "Сотрудник". 


  /* Номер кред.соглашения  */
  display loan.cont-code format "x(15)" column-label "№ КС".

  /* Дата заключения КС. */
  display loan.open-date column-label "Дата!заключения КС".

  /*%ставкаи */
  r-bad = 0.
  r-bad = decimal(GetXattrValueEx("loan",
                                  loan.contract + "," + loan.cont-code,
                                  "Доп_проц",
                                  ?)) no-error.

  display
    /*%ставка по ср.задолж. */
    GET_COMM ("%Кред",
              ?,
              loan.currency,
              loan.contract + "," + loan.cont-code,
              0.0,
              0,
              end-date)
    format ">>>9.9999" column-label "% ставка!по ср.!задолж."

    /* пр.ставка зад.при невыполнении условий */
    r-bad
    format ">>>9.9999" column-label "% ставка!по ср.!зад.при!невып.!усл."
    when r-bad ge 0

    /* %ставка по проср.задолж. */
    GET_COMM ("%КрПр",
              ?,
              loan.currency,
              loan.contract + "," + loan.cont-code,
              0.0,
              0,
              end-date)
    format ">>>9.9999" column-label "% ставка!по проср.!задолж."
  .

  /* срок погашения */
  display loan.end-date column-label "Срок!погашения".

  /* Дата уплаты процентов */
  find last loan-cond where loan-cond.contract = loan.contract
                        and loan-cond.cont-code = loan.cont-code
                        and loan-cond.since le end-date no-lock no-error.
  
  if avail loan-cond then do:
    display  
      (if loan-cond.int-period ne "П"
      then string((loan-cond.int-date + loan-cond.delay) modulo 31 , ">9")
      else "  ")
      format "x(2)" column-label "Дата!уплаты %"
    .
  end.

  /*----------------------------------------------------------------  
      Лимит кредитования в $
   ---------------------------------------------------------------*/
  if avail loan-cond then do:
    /* Commented by KSV: Информация о лимите кредитования для течений выводится
    ** в охватывающем договоре, т.о. избегаем проблем с задвоением итогов */
    IF NOT loan.cont-code MATCHES "* *" THEN
       run RE_PLAN_SUMM_BY_LOAN in h_loan (loan-cond.contract,
                                           loan-cond.cont-code,
                                           loan-cond.since,
                                           output s-limit).
    ELSE
       s-limit = 0.
      
    if  GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "Груп_Дог",?) = "1"
    then do:
      /* дата соглашения */
      assign ds = date (GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "ДатаСогл",?)) no-error. 
      if ds eq ? then do:
        put unformatted skip(2) "Ошибка!!! Не введено значение доп.реквизита ДатаСогл!" skip(2) .
      end. else do:
        /* пересчет в $ лимита */
        s-limit$ = CurToCur("Учетный", loan.currency, "840", ds, s-limit). 
        r$ = findRate("Учетный", "840", ds).

        display s-limit$  format "->>>>>>>>>>>9.99"  column-label "Лимит!кредитования,$"
                r$        format "->>>>>9.9999"      column-label "Курс $!на дату!реш-я".
        accumulate s-limit$ (total).
      end.
    end.

      /*----------------------------------------------------------------
         Ссудн.задолженость и лимит кредитования в рублях.
      ----------------------------------------------------------------*/
      def var params as decimal extent 3 no-undo.  /* для вычисления 0 7 13 параметров договора */
      params = 0.
      run param_0      in h_loan (loan.contract, loan.cont-code,  0, end-date, output params[1], output db-tmp, output cr-tmp).
      run stndrt_param in h_loan (loan.contract, loan.cont-code,  7, end-date, output params[2], output db-tmp, output cr-tmp).
      run param_13     in h_loan (loan.contract, loan.cont-code, 13, end-date, output params[3], output db-tmp, output cr-tmp).
            
      params[1] = params[1] + params[2] + params[3].

      /* Сумма задолженности и лимит кредитования в рублях по курсу ЦБ
         на день получения ведомости */

      if loan.currency ne "" then do:
        r$ = findRate("Учетный", loan.currency, end-date). 
        params[1] = round(params[1] * r$, 2).
        s-limit   = round(s-limit * r$, 2).
      end.

      display params[1] format "->>>>>>>>>9.99"  column-label "Сумма!задолженности"
              s-limit   format "->>>>>>>>>9.99"  column-label "Лимит!кредитования"  .

     display  GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"ЦельКред", ?) format "x(10)" column-label "Цель".
     display  GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"ПрогКред", ?) format "x(20)" column-label "ПрогКред".
     display  GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"КП_ИФ", ?) format "x(20)" column-label "Канал продаж".
     display  GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"СоглСтрахЖизн", ?) format "x(5)" column-label "Страх".

      accumulate params[1] (total)
                 s-limit (total) .

     if last(loan.cont-code) then do:
       underline name
                 s-limit$
                 params[1]
                 s-limit .

       down.
       display "Итого:" @ name
                accum total s-limit$ @ s-limit$
                accum total params[1] @ params[1]
                accum total s-limit @ s-limit .

     end.
  end. /* avail loan-cond */
end.
/*{signatur.i}*/
{endout3.i &nofooter=yes}

