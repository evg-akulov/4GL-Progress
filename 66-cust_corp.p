/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: ��� ��������������
      Filename: 
      Comment: 
   Parameters
         Use:
      Used by:
      Created: 24/01/2013
     Modified: 29/01/2013 - ��������� �஢�ઠ 455*
     Modified: 31/01/2013 - �஢�ઠ �� �������� 455*
*/

{globals.i}
{tmprecid.def}


DEF VAR klient       AS CHAR NO-UNDO.
DEF VAR teleph       AS CHAR NO-UNDO.
DEF VAR mAcct1       AS CHAR NO-UNDO.
DEF VAR vTurnCr1     AS DECIMAL NO-UNDO.

put unformatted "12��������� �����⮢" skip. 
PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED "��� ������;������������;����䮭;" SKIP.  


for each tmprecid no-lock, 
    first cust-corp where recid(cust-corp) = tmprecid.id no-lock:
       klient = string(cust-corp.cust-id) + ";" + cust-corp.name-corp + ";".
       teleph = GetXattrValueEx("cust-corp",STRING(cust-corp.cust-id),"tel", ?).
       PUT UNFORMATTED STRING(klient,"x(200)") ";" teleph ";".    
end.



PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}