/*
               Банковская интегрированная система БИСквит
    Copyright: Алтайский РФ ОАО "Россельхозбанк"
     Filename: ved-proc1.p
      Comment: Отчет по процентам на ссудную задолженность по портфелям и договорам,
               и остаткам для погашения   
   Parameters: 
         Uses: ost-cli.i
      Used by:
      Created: 14/04/2006  //Перьков Д.В. т.(3852) 36-99-89 вн.1510 
     Modified: 10/11/2006  //Магомерзаев И.В. т.(3852) 36-99-89 вн.1510
                           //разделен расчет выполнения обязательств 
                           //на на основной долг и проценты
               17/11/2006  //Перьков Д.В. т.(3852) 36-99-89 вн.1510
                           // ограничение видимости для нижестоящих подразделений согласно правам доступа
               05/09/2008  //Паутов С.А. т.(3852) 36-99-89 вн.1512
                           // новая форма отчета
*/

DEF  INPUT  PARAMETER s-param   AS INT        NO-UNDO.

{globals.i}
{tmprecid.def}
{setdest.i}
{sh-defs.i}

DEFINE var iTitle  AS CHARACTER  init "Выберите портфели" NO-UNDO.
DEFINE var iLevel  AS INTEGER    init 5   NO-UNDO.

end-date = end-date - 1.

{18ost-cli.i}
{18dayinyear.i}

DEFINE TEMP-TABLE ttLOAN 
   FIELD contract   LIKE loan.contract
   FIELD cont-code  LIKE loan.cont-code
   FIELD cust-cat   LIKE loan.cust-cat
   FIELD cust-id    LIKE loan.cust-id
   FIELD portfel     AS CHARACTER 
   FIELD nameclient  AS CHARACTER 
   FIELD birthclient AS CHARACTER 
   FIELD pfname     AS CHARACTER
   FIELD raion      AS CHARACTER 
   FIELD branch-id  AS CHARACTER 
   FIELD sroch-dolg LIKE op-entry.amt-rub
   FIELD osn-dolg   LIKE op-entry.amt-rub
   FIELD proc-dolg  LIKE op-entry.amt-rub
   FIELD procvn-dolg LIKE op-entry.amt-rub
   FIELD ost-rasch  LIKE op-entry.amt-rub
   FIELD ost-rasch2 LIKE op-entry.amt-rub
   FIELD cont-type  LIKE loan.cont-type
   FIELD stavka-cred LIKE op-entry.amt-rub
   FIELD date-osn-dolg    AS DATE
   FIELD date-proc-dolg   AS DATE
   FIELD date-procvn-dolg AS DATE
   FIELD KDP    AS INT
   FIELD acct-kredvozmub  AS CHARACTER
   FIELD lastopdate  AS CHARACTER 
   FIELD Kol_DN  AS INT 
   FIELD Kol_DNP  AS INT 
index i1 portfel.

DEFINE TEMP-TABLE ttLOAN_svod
   FIELD pfname AS CHARACTER
   FIELD sroch-dolg AS DEC
   FIELD osn-dolg   AS DEC
   FIELD proc-dolg  AS DEC
   FIELD procvn-dolg AS DEC
index i1 pfname.   
.


DEFINE TEMP-TABLE ttLOANOST 
   FIELD cust-cat   LIKE loan.cust-cat
   FIELD cust-id    LIKE loan.cust-id
   FIELD ost        LIKE op-entry.amt-rub
index i1 cust-id.

DEFINE NEW SHARED TEMP-TABLE ttSPISOK 
   FIELD pname     like code.name
   FIELD pcode     like code.code.

DEFINE NEW SHARED TEMP-TABLE ttBranch
   FIELD branch-id like branch.branch-id
   FIELD name      like branch.name.

DEFINE NEW SHARED TEMP-TABLE ttMONEY 
   FIELD name     like code.name
   FIELD code     like code.code.

DEFINE TEMP-TABLE tmp-op-entry
   FIELD op-date   LIKE op-entry.op-date
   FIELD acct-cat  AS CHARACTER
   FIELD amt-rub   LIKE op-entry.amt-rub
index i1 op-date.

end-date = gend-date.

define var vCountInt   as int     init 0 no-undo. /* Счетчик обработанных записей. */
define var vTotalInt   as int     no-undo. /* Общее количество записей. */
define var name-klient as char    no-undo.
define var namep       as char    no-undo.
define var t_acct      as char    format "x(20)"  no-undo.
define var FILIAL      as char    format "x(20)"  no-undo.
define var maskacc     as char    format "x(500)" no-undo.
define var in-portf    as char    format "x(400)" init "*" no-undo.
define var in-branch   as char    format "x(400)" init "*" no-undo.
define var have_money  as char    format "x(20)"  init "Все договора" no-undo.
define var od          as date no-undo.
define var have_double as char    format "x(1)"   init "" no-undo.
define var have_rigths as char    no-undo.
define var ss          as char    no-undo.
define var i           as integer no-undo.

DEF VAR  raion            AS CHAR NO-UNDO.
DEF VAR  adress           AS CHAR NO-UNDO.
DEF VAR  tot1             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  tot2             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  tot3             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  tot4             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  tot5             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  tot6             LIKE op-entry.amt-rub init 0 no-undo.  
DEF VAR  tot7             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  tot8             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  it1              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  it2              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  it3              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  it4              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  it5              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  it6              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  it7              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  it8              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  s1               LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  s1i              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  s1p              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  s2               LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  s3               LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  s4               LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  its1              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  its2              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  its3              LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  its4              LIKE op-entry.amt-rub init 0 no-undo.


DEF VAR  s1p1             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  s1p2             LIKE op-entry.amt-rub init 0 no-undo.

DEF VAR  s1i1             LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  s1i2             LIKE op-entry.amt-rub init 0 no-undo.

DEF VAR  sroch-dolg       LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  osn-dolg         LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  proc-dolg        LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  procvn-dolg      LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  ost-rasch        LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  ost-rasch2       LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  t_ost            LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  obesp-ost        LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  obesp-tmp-ost    LIKE op-entry.amt-rub init 0 no-undo.

DEF VAR  date-osn-dolg    AS DATE no-undo.
DEF VAR  date-proc-dolg   AS DATE no-undo.
DEF VAR  date-procvn-dolg AS DATE no-undo.

DEF VAR  person-name      AS CHARACTER no-undo.
DEF VAR  mKredVozmUb      AS CHARACTER no-undo.
DEF VAR  mLastOpDate      AS DATE no-undo.

DEF VAR  ds1              LIKE op-entry.amt-rub init 0 no-undo.


DEF VAR  cred-ost         LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  stavka-cred      LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  proci            LIKE op-entry.amt-rub init 0 no-undo.


DEF VAR  kol-dayi         AS INT no-undo.
DEF VAR  DayInYearI       AS INT no-undo.
DEF VAR  rk-date          AS DATE no-undo.
DEF VAR  birthday-client  AS CHARACTER NO-UNDO.

DEF VAR  check-ost1     LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  check-ost2     LIKE op-entry.amt-rub init 0 no-undo.
DEF VAR  check-ost3     LIKE op-entry.amt-rub init 0 no-undo.

DEF VAR Kol_DN AS INT.
DEF VAR Kol_DNP AS INT.
DEF VAR MPL1 AS DEC.
DEF VAR MPL2 AS DEC.
DEF VAR Sum_SZ AS DECIMAL NO-UNDO.
DEF VAR si1 AS DEC.
DEF VAR si2 AS DEC.

DEF BUFFER buf-loan-acct FOR loan-acct.
DEF BUFFER buf2-loan-acct FOR loan-acct.
DEF BUFFER buf3-loan-acct FOR loan-acct.

function check_branch returns LOGICAL (input acc as char, input mask as char).
  def var i as integer no-undo.

  if (index(mask,"*") > 0 ) THEN return true.
  do i = 1 to NUM-ENTRIES(mask):
   if acc matches '.........' + entry(i,mask) + '.......' THEN return true.
  END.
  return false.
END.

/* заполнение временных таблиц */
CREATE ttBranch.
assign 
 ttBranch.branch-id = "*"
 ttBranch.name      = "все подразделения"
.
FILIAL      = FGetSetting("КодФил", ?,"6600"). /* отбираем ДО по нехитрому принципу */
have_rigths = GetXAttrValueEx ("_user",userid("bisquit"),"Отделение",""). /* права доступа к данным других подразделений */ 
i = 0.
for each branch where ((branch.branch-id = FILIAL and branch.parent-id = 'Top') OR
                      (branch.parent-id = FILIAL and branch.branch-id matches '....')) and
                      can-do(have_rigths,branch.branch-id) no-lock:
    CREATE ttBranch.
    assign 
     ttBranch.branch-id = branch.branch-id
     ttBranch.name      = branch.name
     i                  = i + 1
     ss                 = branch.branch-id
    .
    release ttBranch.
end.
release ttBranch.
if i = 1 then in-branch = ss. /* права доступа только к одному подразделению, сразу его показываем */

for each code where code.class = "UnifrmBag" and code.code begins "ПФ" no-lock:
    CREATE ttSPISOK. /* только портфели по кредитам физических лиц */
    assign 
     ttSPISOK.pname = code.name
     ttSPISOK.pcode = code.code
    .
    release ttSPISOK.
end.
/*----*/
    CREATE ttMONEY.
    assign 
     ttMONEY.name = "Средств недостаточно"
     ttMONEY.code = "-"
    .
    release ttMONEY.
    CREATE ttMONEY.
    assign 
     ttMONEY.name = "Средств достаточно"
     ttMONEY.code = "+"
    .
    release ttMONEY.
    CREATE ttMONEY.
    assign 
     ttMONEY.name = "Все договора"
     ttMONEY.code = "*"
    .
    release ttMONEY.

RUN get-data.
od = end-date.

IF keyfunc(lastkey) = "END-ERROR" THEN RETURN.

def var fs  as char no-undo.
fs  = "ved-proc15" + string(time) + ".txt".
{setdest.i &filename = "fs"
           &custom = "IF TRUE THEN 0 ELSE"}
/*{strtout3.i &custom = "IF TRUE THEN 0 ELSE"}*/
PUT UNFORMATTED Fgetsetting("Банк",?,"") SKIP.
PUT UNFORMATTED "Ведомость - расшифровка по задолженности по портфелям кредитных договоров физических лиц" 
                + " с 01" + SUBSTR(STRING(end-date,"99/99/9999"),3) + " по " + STRING(end-date,"99/99/9999") SKIP.  
PUT UNFORMATTED "Дата и время формирования отчета " + 
                STRING(today,"99/99/9999") + "  " + 
                STRING(time,"hh:mm:ss") SKIP(1).

{justasec}
FOR each loan where loan.contract EQ "кредит"
                AND loan.close-date EQ ?
                and loan.cust-cat = "Ю" NO-LOCK:

   assign
      sroch-dolg  = 0
      osn-dolg    = 0
      proc-dolg   = 0
      procvn-dolg = 0
      ost-rasch   = 0
      ost-rasch2  = 0
      mLastOpDate = ?
      date-osn-dolg    = ?
      date-proc-dolg   = ?
      date-procvn-dolg = ?.

   RUN get-ost (LOAN.cont-code,"Кредит",   output sroch-dolg).
   RUN get-ost (LOAN.cont-code,"КредПр",   output osn-dolg).
   RUN get-ost (LOAN.cont-code,"КредПр%",  output proc-dolg).
   RUN get-ost (LOAN.cont-code,"КредПр%В", output procvn-dolg).
   /* даты выхода на просрочку */
   RUN get-date-out (LOAN.cont-code,"КредПр",   output date-osn-dolg).
   RUN get-date-out (LOAN.cont-code,"КредПр%",  output date-proc-dolg).
   RUN get-date-out (LOAN.cont-code,"КредПр%В", output date-procvn-dolg).


    for first comm-rate where kau = loan.contract + "," + loan.cont-code and commission = "%Кред" 
      use-index kau no-lock:
      stavka-cred = comm-rate.rate-comm. 
    end.

   mKredVozmUb = "".

Kol_DN = end-date - date-osn-dolg.
Kol_DNP = end-date - date-proc-dolg.

/*    run RE_CLIENT (loan.cust-cat,loan.cust-id,input-output name-klient).*/

    CREATE ttLOAN.
    ASSIGN  ttLOAN.contract  = loan.contract
            ttLOAN.cont-code = loan.cont-code
            ttLOAN.cust-cat  = loan.cust-cat
            ttLOAN.cust-id   = loan.cust-id
            ttLOAN.portfel   = "-"
            ttLOAN.pfname    =  GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"ПрогКред", ?)
            ttLOAN.nameclient  = name-klient
            ttLOAN.birthclient = birthday-client
            ttLOAN.raion       = raion
            ttLOAN.branch-id   = loan.branch-id
            ttLOAN.stavka-cred = stavka-cred
            ttLOAN.cont-type   = loan.cont-type
            ttLOAN.sroch-dolg  = sroch-dolg
            ttLOAN.osn-dolg    = osn-dolg
            ttLOAN.proc-dolg   = proc-dolg
            ttLOAN.procvn-dolg = procvn-dolg
            ttLOAN.ost-rasch   = ost-rasch
            ttLOAN.ost-rasch2  = ost-rasch2
            ttLOAN.date-osn-dolg    = date-osn-dolg
            ttLOAN.date-proc-dolg   = date-proc-dolg
            ttLOAN.date-procvn-dolg = date-procvn-dolg
            ttLOAN.KDP = Kol_DN
            ttLOAN.acct-kredvozmub  = mKredVozmUb
            ttLOAN.lastopdate       = IF mLastOpDate = ? THEN "" ELSE STRING(mLastOpDate,"99/99/9999")
            ttLOAN.Kol_DN      = Kol_DN
            ttLOAN.Kol_DNP     = Kol_DNP
            vTotalInt          = vTotalInt + 1.

    RELEASE ttLOAN.

END.
/*Сумма Кредитного портфеля по филиалу*/
Sum_SZ = 0.
FOR EACH acct WHERE 
  CAN-DO("458*,4550*",TRIM(acct.acct))
  AND acct.acct-cat EQ "b"
  AND acct.close-date EQ ? NO-LOCK:
	RUN acct-pos in h_base (acct.acct, acct.currency, od, od, ?).
	Sum_SZ = Sum_SZ + ABS(sh-bal).
END.
PUT UNFORMATTED "===================================================================" SKIP.
PUT UNFORMATTED "Кредитный портфель филиала на дату " od Sum_SZ  format "->>,>>>,>>>,>>9.99" " руб." SKIP.
PUT UNFORMATTED "сумма по счетам 4550*,458*" SKIP.
PUT UNFORMATTED "===================================================================" SKIP.

PUT UNFORMATTED "СВОДНАЯ ИНФОРМАЦИЯ" SKIP.

tot1 = 0. tot2 = 0. tot3 = 0. tot4 = 0. tot5 = 0.

for each ttLOAN break by ttLOAN.branch-id:

   if FIRST-OF(ttLOAN.branch-id) THEN DO:
    it1 = 0. it2 = 0. it3 = 0. it4 = 0. it5 = 0. it6 = 0. it7 = 0. MPL1 = 0. MPL2 = 0. /* итоги по портфелю */
    EMPTY TEMP-TABLE ttLOAN_svod.
    
    PUT UNFORMATTED "ДО " ttLOAN.branch-id SKIP.
    PUT UNFORMATTED " ПрогКред       Срочная            Задолженность      Итого        Доля             Доля           " SKIP
                    "                задолженность      по осн.долгу       задолж       просрочки в ДО   просрочки в ИРФ" SKIP.
   END.

   s1 = ttLOAN.sroch-dolg + ttLOAN.osn-dolg.
   s2 = abs(ttLOAN.ost-rasch - s1).
   s3 = ttLOAN.sroch-dolg + ttLOAN.osn-dolg + ttLOAN.proc-dolg.

   IF ttLOAN.Kol_DN > 90 OR ttLOAN.Kol_DNP > 90 THEN DO:
    CREATE ttLOAN_svod.
    ASSIGN  ttLOAN_svod.pfname      = ttLOAN.pfname
            ttLOAN_svod.sroch-dolg  = ttLOAN.sroch-dolg
            ttLOAN_svod.osn-dolg    = ttLOAN.osn-dolg
            ttLOAN_svod.proc-dolg   = ttLOAN.proc-dolg.
    RELEASE ttLOAN_svod.
  END.

      assign
        it1 = it1 + ttLOAN.sroch-dolg
        it2 = it2 + ttLOAN.osn-dolg
        it3 = it3 + ttLOAN.proc-dolg
        it5 = it5 + s1
        it6 = it6 + s3
        MPL1 = it5 / it6.
        MPL2 = (it5 / Sum_SZ) * 100.

   IF LAST-OF(ttLOAN.branch-id) THEN DO:

      for each ttLOAN_svod break by ttLOAN_svod.pfname:

          if FIRST-OF(ttLOAN_svod.pfname) THEN DO:
            its1 = 0. its2 = 0. its3 = 0. its4 = 0. /* итоги по портфелю */
          END.          

          assign
            its1 = its1 + ttLOAN_svod.sroch-dolg
            its2 = its2 + ttLOAN_svod.osn-dolg
            its3 = its3 + ttLOAN_svod.proc-dolg.

          if LAST-OF(ttLOAN_svod.pfname) THEN DO:
               si1 = its1 + its2.
               si2 = its1 + its2 + its3.
               PUT UNFORMATTED
                     ttLOAN_svod.pfname  format "x(12)" " " 
                     its1                       format ">>>,>>>,>>9.99"
                     its2                       format "->>,>>>,>>>,>>9.99"
                     si1                        format "->>,>>>,>>>,>>9.99"
                     si1 / si2                  format ">>>>>>>>>>9.99"
                     (si1 / Sum_SZ) * 100         format ">>>>>>>>>>9.9999"         SKIP.
          end.

      end.


     PUT UNFORMATTED
                     ""  format "x(12)" " " 
                     it1 format ">>>,>>>,>>9.99"
                     it2 format "->>,>>>,>>>,>>9.99"
                     it5 format "->>,>>>,>>>,>>9.99"
                     MPL1 format ">>>>>>>>>>9.99"
                     MPL2 format ">>>>>>>>>>9.9999"
            SKIP(1).
     assign
        tot1 = tot1 + it1
        tot2 = tot2 + it2
        tot5 = tot5 + it5.
   END.
end.  
PUT UNFORMATTED "ИТОГО" SKIP.
PUT UNFORMATTED
                     ""  format "x(12)" " "               
                     tot1 format "->>,>>>,>>>,>>9.99"
                     tot2 format "->>,>>>,>>>,>>9.99"
                     tot5 format "->>,>>>,>>>,>>9.99"
                     ""   format ">>>>>>>>>>9.99"
                     (tot5 / Sum_SZ) * 100 format ">>>>>>>>>>9.9999" SKIP(2).

PackagePrint = false.
{preview.i &filename = "fs"}
/*{endout3.i}*/

PROCEDURE get-data.

   PAUSE 0.
   DEF FRAME ftune
       in-branch  VIEW-AS FILL-IN SIZE 20 BY 1 LABEL "Выберите подразделение" HELP "Если * то по всем"
       in-portf   VIEW-AS FILL-IN SIZE 20 BY 1 LABEL "Выберите портфель" HELP "Портфель однородных ссуд"
       have_money VIEW-AS FILL-IN SIZE 20 BY 1 LABEL "Выберите портфель" HELP "Хватает средств или нет"
       end-date   LABEL "За какой день"   
                  HELP "Введите дату расчета (F1 - календарь)"
       WITH side-labels centered overlay row 7 1 col
       TITLE COLOR BRIGHT-WHITE "[ ВЫБЕРИТЕ ПОРТФЕЛИ ]".

   ON RETURN OF END-DATE IN FRAME ftune DO:
      DISPLAY INPUT end-date @ end-date WITH FRAME FTUNE.
   END.

   ON F1 OF in-portf IN FRAME FTUNE DO:

      run "18nav_spis.p" (?).
      IF (LASTKEY = 13 or lastkey = 10) AND PICK-VALUE <> ? THEN
         SELF:SCREEN-VALUE = PICK-VALUE.
      RETURN NO-APPLY.
   END.

   ON F1 OF in-branch IN FRAME FTUNE DO:

      run "18nav_brnch.p" (?).
      IF (LASTKEY = 13 or lastkey = 10) AND PICK-VALUE <> ? THEN
         SELF:SCREEN-VALUE = PICK-VALUE.
      RETURN NO-APPLY.
   END.
   
   ON F1 OF have_money IN FRAME FTUNE DO:

      run "18nav_mony.p" (?).
      IF (LASTKEY = 13 or lastkey = 10) AND PICK-VALUE <> ? THEN
         SELF:SCREEN-VALUE = PICK-VALUE.
      RETURN NO-APPLY.
   END.

   ON F1 OF END-DATE IN FRAME ftune DO:
      RUN calend.p.
      IF (LASTKEY = 13 OR LASTKEY = 10) AND PICK-VALUE <> ? THEN
         SELF:SCREEN-VALUE = PICK-VALUE.
      RETURN NO-APPLY.
   END.

   DO TRANSACTION ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
        UPDATE
           in-branch
           in-portf
           have_money
           end-date
        WITH FRAME ftune.
   END.
   HIDE FRAME ftune NO-PAUSE.
   IF KEYFUNC(LASTKEY) = "END-ERROR" THEN RETURN.
END PROCEDURE.

PROCEDURE get-ost:
DEFINE INPUT PARAMETER loan-cont-code AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER loan-acct-role AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER acct-ost AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE ost  AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE BUFFER bf-loan-acct FOR loan-acct.

   RELEASE bf-loan-acct.
   FOR LAST bf-loan-acct where
            bf-loan-acct.contract = "Кредит"
        and bf-loan-acct.cont-code = loan-cont-code
        and bf-loan-acct.acct-type = loan-acct-role no-lock,
       FIRST acct OF bf-loan-acct NO-LOCK:
          RUN "18ostatok.p" (bf-loan-acct.acct, bf-loan-acct.currency, od, "√,√√", output ost).
          acct-ost = abs(ost).
   END.
END.


/* дата выхода на просрочку */
PROCEDURE get-date-out:
DEFINE INPUT PARAMETER loan-cont-code AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER loan-acct-role AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER date-out AS DATE INITIAL ? NO-UNDO.
DEFINE VARIABLE ost  AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE BUFFER bf-loan-acct FOR loan-acct.

   RELEASE bf-loan-acct.
   FOR EACH tmp-op-entry:
       DELETE tmp-op-entry.
   END.

   FIND LAST bf-loan-acct where
             bf-loan-acct.contract = "Кредит"
         and bf-loan-acct.cont-code = loan-cont-code
         and bf-loan-acct.acct-type = loan-acct-role NO-LOCK NO-ERROR.
    IF AVAILABLE bf-loan-acct THEN DO:
     /* пройдем по дебету */
     FOR each op-entry where
              op-entry.acct-db = bf-loan-acct.acct
          and op-entry.op-status begins '√'
          and op-entry.op-date >= bf-loan-acct.since
          and op-entry.op-date <= od no-lock:
        create tmp-op-entry.
        assign tmp-op-entry.op-date  = op-entry.op-date
               tmp-op-entry.acct-cat = "DB"
               tmp-op-entry.amt-rub  = op-entry.amt-rub.
     END.
     /* пройдем по кредиту */
     FOR each op-entry where
              op-entry.acct-cr  = bf-loan-acct.acct
          and op-entry.op-status begins '√'
          and op-entry.op-date >= bf-loan-acct.since
          and op-entry.op-date <= od no-lock:

        create tmp-op-entry.
        assign tmp-op-entry.op-date  = op-entry.op-date
               tmp-op-entry.acct-cat = "CR"
               tmp-op-entry.amt-rub  = op-entry.amt-rub.
     END.
      /* теперь пройдем по созданной таблице */
      FOR EACH tmp-op-entry USE-INDEX i1 NO-LOCK :
         if tmp-op-entry.acct-cat = "DB" then do:
            if date-out = ? then
               date-out = tmp-op-entry.op-date.
            ost = ost + tmp-op-entry.amt-rub.
         end.

         if tmp-op-entry.acct-cat = "CR" then
               ost = ost - tmp-op-entry.amt-rub.

         if ost <= 0 then
            date-out = ?.
      END.

    END.
END.
