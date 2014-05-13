{globals.i}
{tmprecid.def}
{setdest.i}


put unformatted "Ведомость клиентов" skip. 
PUT UNFORMATTED "" SKIP(2).


FOR EACH TmpObj WHERE NO-LOCK
FIRST acct WHERE RECID(acct) TmpObj.rid NO-LOCK:

	PUT UNFORMATTED acct.acct SKIP.  

end.     






