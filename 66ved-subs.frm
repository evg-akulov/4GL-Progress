/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2006 ЗАО "Банковские информационные системы"
     Filename: ved-subs.frm
      Comment: (0072887) Печатные формы для справки-уведомления об остатке 
                  ссудной задолженности и  о начисленных и уплаченных процентах
   Parameters:
         Uses:
      Used by:
      Created: 22.01.2007  Ariz
*/

&GLOBAL-DEFINE form1-width 220
&GLOBAL-DEFINE form2-width 340
&GLOBAL-DEFINE form3-width 200

/* Форма 1 */
DEFINE FRAME form1
   "│"
   RepInfo.nn
      FORMAT ">>>9"
   "│"      
   RepInfo.ClientName
      FORMAT "X(20)"
   "│"      
   RepInfo.datenum
      FORMAT "X(25)"
   "│"      
   RepInfo.acct
      FORMAT "X(20)"
   "│"      
   LoanInfo.bal-summ
      FORMAT ">>,>>>,>>>,>>9.99"
   "│"      
   LoanInfo.rat-cred
      FORMAT ">>>>>>9.99"
   "│"      
   RepInfo.rat-ref
      FORMAT ">>>>>>>>>>>>>>>9.99"
   "│"      
   LoanInfo.rat1
      FORMAT " >>>,>>9.99"
   "│"      
   LoanInfo.summ_pr
      FORMAT ">>>,>>>,>>9.99"
   "│"      
   LoanInfo.ndays
      FORMAT ">>>>>>>>>>9"
   "│"
   LoanInfo.beg-date
      FORMAT "99.99.9999"
   "│"
   LoanInfo.end-date
      FORMAT "99.99.9999"
   "│"   
WITH WIDTH {&form1-width} DOWN NO-BOX.

/* Форма 2 */
DEFINE FRAME form2
   ";"
   RepInfo.nn
      FORMAT ">>9"
   ";"      
   RepInfo.ClientName
      FORMAT "X(45)"
   ";"      
   RepInfo.datenum
      FORMAT "X(25)"
   ";"      
   RepInfo.acct
      FORMAT "X(20)"
   ";"      
   LoanInfo.bal-summ
      FORMAT ">>,>>>,>>>,>>9.99"
   ";"      
   LoanInfo.rat-cred
      FORMAT ">>>>>>9.99"
   ";"      
   RepInfo.rat-ref
      FORMAT ">>>>>>>>>>>>>>>9.99"
   ";"      
   LoanInfo.rat1
      FORMAT " >>>,>>9.99"
   ";"      
   LoanInfo.summ_pr
      FORMAT ">>>,>>>,>>9.99"
   ";"      
   LoanInfo.ndays
      FORMAT ">>>>>>>>>>9"
   ";"
   LoanInfo.beg-date
      FORMAT "99.99.9999"
   ";"
   LoanInfo.end-date
      FORMAT "99.99.9999"
   ";" 
   LoanInfo.Raion
      FORMAT "X(15)"
   ";"

WITH WIDTH {&form2-width} DOWN NO-BOX.

/* форма ошибок */
DEFINE FRAME form3
   "│"
   errinfo.number
      FORMAT ">>>9"
   "│"
   errinfo.dog-num
      FORMAT "x(25)" 
   "│"
   errinfo.mess
      FORMAT "x(110)"
   "│"
WITH NO-LABELS WIDTH {&form3-width} NO-BOX DOWN.

DEFINE FRAME FormNum
    iNumFrmSel
          HELP  "Введите номер формы ( 1 или 2 )."
          LABEL "Номер формы"
WITH OVERLAY SIDE-LABELS 1 COL CENTERED ROW 6 .
