/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2003 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: vtb-lim.p
      Comment: ����ﭨ� �����, ��㤭�� ������������� �� ���ﭨ� �� .... 
   Parameters: 
         Uses:                     
      Used by:                     
      Created:
     Modified: 08.08.2003 15:17 KSV      (0010770) ��ࠢ���� ��䮣���᪠�
                                         �訡�� � ��������� ����. ��ࠢ����
                                         �訡�� ��������� �⮣� �� ������
                                         �।�⮢����.
     Modified: 04.03.2011 16:46 krag     <comment>
*/

{globals.i}
{getdate.i}
{49base.fun}
{49loan.fun}
{setdest.i}
end-date = end-date - 1.
{intrface.get comm}
{intrface.get xclass}
{intrface.get loan}
{intrface.get instrum}
{intrface.get i254}
{sh-defs.i}
{tmprecid.def}


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
DEF VAR mAcct1       AS CHAR NO-UNDO.
DEF VAR mAcct2       AS CHAR NO-UNDO.
DEF VAR mAcct3       AS CHAR NO-UNDO.
DEF VAR mAcct4       AS CHAR NO-UNDO.
DEF VAR mAcct5       AS CHAR NO-UNDO.
DEF VAR mAcct6       AS CHAR NO-UNDO.
DEF VAR mAcct7       AS CHAR NO-UNDO.
DEF VAR mAcct8       AS CHAR NO-UNDO.
DEF VAR mAcct9       AS CHAR NO-UNDO.
DEF VAR vTurnCr1     AS DECIMAL NO-UNDO.
DEF VAR vTurnCr2     AS DECIMAL NO-UNDO.
DEF VAR vTurnCr3     AS DECIMAL NO-UNDO.
DEF VAR vTurnCr4     AS DECIMAL NO-UNDO.
DEF VAR vTurnCr5     AS DECIMAL NO-UNDO.
DEF VAR vTurnCr6     AS DECIMAL NO-UNDO.
DEF VAR vTurnCr7     AS DECIMAL NO-UNDO.
DEF VAR vTurnCr8     AS DECIMAL NO-UNDO.
DEF VAR vTurnCr9     AS DECIMAL NO-UNDO.
DEF VAR acct-ost     AS DECIMAL NO-UNDO. /* ���⮪ �� ���.��� */
DEF VAR vSum         AS DECIMAL NO-UNDO.
DEF VAR vStr         AS CHAR NO-UNDO.
DEF VAR mGrRiska     AS INT64.
DEF VAR teleph       AS CHAR NO-UNDO.


FUNCTION GetSaldo_acct RETURNS DECIMAL(n_acct AS CHAR,val_acct AS CHAR):
  DEF VAR tmpResult AS DECIMAL NO-UNDO.
  tmpResult = 0.00.

   RUN acct-pos in h_base (n_acct, val_acct, end-date, end-date, ?).
   tmpResult = ABS(sh-bal).

  RETURN tmpResult.
END FUNCTION.


put unformatted " 1 ����ﭨ� �����, ��㤭�� ������������� �� ���ﭨ� �� " (end-date + 1) skip(2).
/*1366011/0545*/
put unformatted "�/�;����騪;⥫�䮭;��;����㤭��;N ��;��� �����祭��;% ��.������.;��.���.��.;��.����.;�ப ����襭��;��� 㯫��� %;����� �।�⮢����;����;�㬬� ������������;����� �।�⮢����;����;�ண�।;����� �த��;����䥫�;����. १��;���.����⢠;4550*;s;45815*;s;45915*;s;47427*;s;45515*;s;45818*;s;45918*;s;47425*;s;91604*;s;"skip.
c = 0.

for each tmprecid no-lock, 
    first loan where recid(loan) = tmprecid.id no-lock:
	  c = c + 1. 
	  put unformatted c ";".

	  /* ����騪 */
	  run GetCustName IN h_base(loan.cust-cat,loan.cust-id,?,
        	                    output name1[1],
                	            output name1[2],
	                            input-output name1[3]).
	  name = trim(name1[1] + " " + name1[2]).
	  put unformatted name ";" .
	  /*⥫�䮭*/
	  teleph = "".
	  find first person where person.person-id = loan.cust-id.
	  assign teleph = "���." + trim(person.phone[1]) + " ���.,���. " + trim(person.phone[2]).
	  put unformatted teleph ";".

	  put unformatted loan.branch-id ";".

	  find first _user 
		where _user._Userid = loan.user-id.
	  put unformatted _user._User-Name ";".

	  /* ����� �।.�������  */
	  put unformatted loan.cont-code ";" .
	  /* ��� �����祭�� ��. */
	  put unformatted loan.open-date ";".

	    /*%�⠢�� �� ��.������. */
	  put unformatted 
	    GET_COMM ("%�।",
	              ?,
	              loan.currency,
	              loan.contract + "," + loan.cont-code,
	              0.0,
	              0,
	              end-date) ";".
	  /*%�⠢��� �� ���믮������ �᫮��� */
	  r-bad = 0.
	  r-bad = decimal(GetXattrValueEx("loan",
        	                          loan.contract + "," + loan.cont-code,
                	                  "���_���",
                        	          ?)) no-error.
	  put unformatted r-bad ";".
	  /* %�⠢�� �� ����.������. */
	  put unformatted 
	    GET_COMM ("%����",
	              ?,
	              loan.currency,
	              loan.contract + "," + loan.cont-code,
	              0.0,
	              0,
	              end-date) ";".
	  /* �ப ����襭�� */
	  put unformatted loan.end-date ";".
	  /* ��� 㯫��� ��業⮢  ����� ��� 
	  find last loan-cond where loan-cond.contract = loan.contract
	                        and loan-cond.cont-code = loan.cont-code
	                        and loan-cond.since le end-date no-lock no-error.
	  if avail loan-cond then do:
		  put unformatted (if loan-cond.int-period ne "�"
		      then string((loan-cond.int-date + loan-cond.delay) modulo 31 , ">9")
		      else "  ") ";" .
		  end.
          end.*/
	  put unformatted  ";".
	  /*����� �।�⮢���� � $*/
	  if avail loan-cond then do:
	    IF NOT loan.cont-code MATCHES "* *" THEN
	       run RE_PLAN_SUMM_BY_LOAN in h_loan (loan-cond.contract,
	                                           loan-cond.cont-code,
	                                           loan-cond.since,
	                                           output s-limit).
	    ELSE
	       s-limit = 0.
      
	    if  GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "���_���",?) = "1"
	    then do:
	      assign ds = date (GetXattrValueEx("loan", loan.contract + "," + loan.cont-code, "��⠑���",?)) no-error. 
	        s-limit$ = CurToCur("����", loan.currency, "840", ds, s-limit). 
	        r$ = findRate("����", "840", ds).
	        accumulate s-limit$ (total).
	    end.
          end.
          put unformatted s-limit$ ";" r$ ";".

      /*----------------------------------------------------------------
         ��㤭.������������ � ����� �।�⮢���� � �㡫��.
      ----------------------------------------------------------------*/
      def var params as decimal extent 3 no-undo.  /* ��� ���᫥��� 0 7 13 ��ࠬ��஢ ������� */
      params = 0.
      run param_0      in h_loan (loan.contract, loan.cont-code,  0, end-date, output params[1], output db-tmp, output cr-tmp).
      run stndrt_param in h_loan (loan.contract, loan.cont-code,  7, end-date, output params[2], output db-tmp, output cr-tmp).
      run param_13     in h_loan (loan.contract, loan.cont-code, 13, end-date, output params[3], output db-tmp, output cr-tmp).
            
      params[1] = params[1] + params[2] + params[3].

      /* �㬬� ������������ � ����� �।�⮢���� � �㡫�� �� ����� ��
         �� ���� ����祭�� �������� */

      if loan.currency ne "" then do:
        r$ = findRate("����", loan.currency, end-date). 
        params[1] = round(params[1] * r$, 2).
        s-limit   = round(s-limit * r$, 2).
      end.

     put unformatted params[1] ";" s-limit ";".

     put unformatted GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"����।", ?) ";".
     put unformatted GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"�ண�।", ?) ";".
     put unformatted GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"��_��", ?) ";".
     put unformatted GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"UniformBag", ?) ";".

  /*13 % १�ࢨ஢����*/
  vSum = LnRsrvRate(loan.contract, loan.cont-code, end-date - 1).
  IF vSum EQ ? THEN 
   vSum = 0.
  PUT UNFORMATTED vSum " ����;".
 
  /*14 - ��㯯� �᪠*/
/*    mGrRiska = LnGetGrRiska(vSum, end-date - 1).
  mGrRiska = loan.gr-riska.*/

  RUN LnGetRiskGrOnDate IN h_i254 (DEC(vSum),loan.open-date,OUTPUT mGrRiska).

  PUT UNFORMATTED mGrRiska " ��;".


  FOR EACH signs WHERE 
           signs.file-name EQ "term-obl" AND 
           signs.code EQ "����ண" AND
           signs.surrogate EQ loan.cont-code
  NO-LOCK:
        message signs.code-value signs.xattr-value
        view-as alert-box.
  end.


/*
	��� ������ ���� term-obl.
	��-�����, ���ண�� �㬠� ⮦� ������, ����� ������ ���� - ���� � ���� ��室���� 49inscomp.p
*/



     mAcct1 = "not".
     mAcct2 = "not".
     mAcct3 = "not".
     mAcct4 = "not".
     mAcct5 = "not".
     mAcct6 = "not".
     mAcct7 = "not".
     mAcct8 = "not".
     mAcct9 = "not".
     vTurnCr1 = 0.00.
     vTurnCr2 = 0.00.
     vTurnCr3 = 0.00.
     vTurnCr4 = 0.00.
     vTurnCr5 = 0.00.
     vTurnCr6 = 0.00.
     vTurnCr7 = 0.00.
     vTurnCr8 = 0.00.
     vTurnCr9 = 0.00.
/* �।��,45815*,45915*,47427*, 45515*,45818*,45918*,47425*,91604*  */
     FOR EACH loan-acct WHERE loan-acct.contract EQ "�।��" AND       
                loan-acct.cont-code EQ loan.cont-code /*AND loan-acct.since LE end-date*/ NO-LOCK:      

		IF loan-acct.acct-type = "�।��" THEN DO:
		       mAcct1 = loan-acct.acct.
		       vTurnCr1 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
		IF substring(loan-acct.acct,1,5) = "45815" THEN DO:
		       mAcct2 = loan-acct.acct.
		       vTurnCr2 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
		IF substring(loan-acct.acct,1,5) = "45915" THEN DO:
		       mAcct3 = loan-acct.acct.
		       vTurnCr3 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
		IF substring(loan-acct.acct,1,5) = "47427" THEN DO:
		       mAcct4 = loan-acct.acct.
		       vTurnCr4 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
		IF substring(loan-acct.acct,1,5) = "45515" THEN DO:
		       mAcct5 = loan-acct.acct.
		       vTurnCr5 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
		IF substring(loan-acct.acct,1,5) = "45818" THEN DO:
		       mAcct6 = loan-acct.acct.
		       vTurnCr6 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
		IF substring(loan-acct.acct,1,5) = "45918" THEN DO:
		       mAcct7 = loan-acct.acct.
		       vTurnCr7 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
		IF substring(loan-acct.acct,1,5) = "47425" THEN DO:
		       mAcct8 = loan-acct.acct.
		       vTurnCr8 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
		IF substring(loan-acct.acct,1,5) = "91604" THEN DO:
		       mAcct9 = loan-acct.acct.
		       vTurnCr9 = GetSaldo_acct(loan-acct.acct,loan-acct.currency).
                END.
     END.	


     put unformatted mAcct1 ";" vTurnCr1 ";" mAcct2 ";" vTurnCr2 ";" mAcct3 ";" vTurnCr3 ";" mAcct4 ";" vTurnCr4 ";" mAcct5 ";" vTurnCr5 ";" mAcct6 ";" vTurnCr6 ";" mAcct7 ";" vTurnCr7 ";" mAcct8 ";" vTurnCr8 ";" mAcct9 ";" vTurnCr9 ";" SKIP.


end.

{preview.i}

