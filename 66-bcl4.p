{globals.i}
{tmprecid.def}
{setdest.i}
{sh-defs.i}

DEF VAR Sum_SZ AS DECIMAL NO-UNDO.

Sum_SZ = 0.
FOR EACH acct WHERE 
  CAN-DO("458*,4550*",TRIM(acct.acct))
  AND acct.acct-cat EQ "b"
  AND acct.close-date EQ ? NO-LOCK:
	RUN acct-pos in h_base (acct.acct, acct.currency, date("22/04/2014"), date("22/04/2014"), ?).
	Sum_SZ = Sum_SZ + ABS(sh-bal).
END.
PUT UNFORMATTED Sum_SZ SKIP.

{preview.i}





