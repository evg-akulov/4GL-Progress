/*
               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2003 ТОО "Банковские информационные системы"
     Filename: 
      Comment: 
   Parameters:
      Created: 
     Modified:
*/
{globals.i}
{tmprecid.def}
{setdest.i}
DEF VAR K AS INT64.
DEF VAR K1 AS INT64.
DEF VAR old_n AS CHAR NO-UNDO.
DEF VAR new_n AS CHAR NO-UNDO.
PUT UNFORMATTED "ПРОТОКОЛ ИЗМЕНЕНИЙ Номеров " SKIP(1).


for each tmprecid no-lock,
first op WHERE RECID(op)  EQ tmprecid.id
exclusive-lock:
 IF LENGTH(op.doc-num) > 3 THEN 
 DO:
   old_n = op.doc-num.
   K1 = LENGTH(op.doc-num).
   K1 = K1 - 2.
   new_n = SUBSTRING (op.doc-num,K1,3).
   op.doc-num = new_n.
   PUT UNFORMATTED "ДОКУМЕНТ №   " old_n "  ИЗМЕНЕН НА " new_n SKIP.
 END.
end.

PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).



{preview.i}

