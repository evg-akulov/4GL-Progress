{globals.i}
{tmprecid.def}
{setdest.i}


put unformatted "��������� ��������" skip. 
PUT UNFORMATTED "" SKIP(2).


FOR EACH TmpObj WHERE NO-LOCK
FIRST acct WHERE RECID(acct) TmpObj.rid NO-LOCK:

	PUT UNFORMATTED acct.acct SKIP.  

end.     






