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

FUNCTION GetClient_Telefon RETURNS CHAR (iCat AS CHAR, iID AS INTEGER):
  DEF VAR tmpResult AS CHAR NO-UNDO.
  tmpResult = "".
  IF iCat EQ "�" THEN DO:
   FIND FIRST person WHERE
              person.person-id EQ iID
   NO-LOCK NO-ERROR.
   IF AVAILABLE person THEN
    tmpResult = "���." + trim(person.phone[1]) + " ���.,���. " + trim(person.phone[2]).
  END.
  IF iCat EQ "�" THEN DO:
   FIND FIRST cust-corp WHERE
              cust-corp.cust-id = iID
   NO-LOCK NO-ERROR.
   IF AVAILABLE cust-corp THEN
    tmpResult = GetXattrValueEx("cust-corp", STRING(cust-corp.cust-id),"tel", ?).
  END.
  RETURN tmpResult.
END FUNCTION.





put unformatted " 1 ����ﭨ� �����, ��㤭�� ������������� �� ���ﭨ� �� " (end-date + 1) skip(2).
/*1366011/0545*/
put unformatted "�/�;����騪;⥫�䮭;��;����㤭��;N ��;��� �����祭��;% ��.������.;��.���.��.;��.����.;�ப ����襭��;��� 㯫��� %;����� �।�⮢����;����;�㬬� ������������;����� �।�⮢����;����;�ண�।;����� �த��;����䥫�;����. १��;���.����⢠;4550*;s;45815*;s;45915*;s;47427*;s;45515*;s;45818*;s;45918*;s;47425*;s;91604*;s;"skip.
c = 0.

for each tmprecid no-lock, 
    first loan where recid(loan) = tmprecid.id no-lock:


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
          put unformatted GetClient_Telefon(loan.cust-cat,loan.cust-id) ";".

	  put unformatted loan.branch-id ";".

	  find first _user 
		where _user._Userid = loan.user-id.
	  put unformatted _user._User-Name ";".

	  /* ����� �।.�������  */
	  put unformatted loan.cont-code ";" .
	  /* ��� �����祭�� ��. */
	  put unformatted loan.open-date ";".


     put unformatted GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"����।", ?) ";".
     put unformatted GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"�ண�।", ?) ";".
     put unformatted GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"��_��", ?) ";".
     put unformatted GetXattrValueEx("loan", loan.contract + "," + loan.cont-code,"UniformBag", ?) ";".
     put unformatted params[1] ";" SKIP.





end.

{preview.i}

