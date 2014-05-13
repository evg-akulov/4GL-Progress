{globals.i}
{sh-defs.i}
{tmprecid.def}

DEFINE VARIABLE mStrTMP  AS CHARACTER NO-UNDO.
DEF VAR loanid    AS CHAR NO-UNDO.
DEF VAR plan_date AS DATE NO-UNDO.
DEF VAR summ_proc AS DEC  NO-UNDO.
DEF VAR summ_osn  AS DEC  NO-UNDO.
DEF VAR vTotal  AS DEC  NO-UNDO.
DEF VAR vSumma  AS DEC  NO-UNDO.
DEF VAR Date   AS CHAR NO-UNDO.
DEF VAR Date2   AS CHAR NO-UNDO.
DEF VAR rpt_str   AS CHAR NO-UNDO.
DEF VAR rpt_str2   AS CHAR NO-UNDO.
DEF VAR in-acct     LIKE acct.acct     NO-UNDO.
DEF VAR in-currency LIKE acct.currency NO-UNDO.
DEF VAR in-date     AS   DATE          NO-UNDO.
DEF VAR inost as dec no-undo.
DEF VAR stat as char no-undo.
DEF VAR fl as logical no-undo.
DEF BUFFER term-obl1 FOR term-obl.

DEF new global shared STREAM slog.

{setdest.i &stream="stream slog" &file="loan_ved_summ.log"}

MESSAGE "‚¢¥¤¨â¥ ¯« ­®¢ãî ¤ âã " UPDATE plan_date.

PUT STREAM slog UNFORMATTED " 			‚…„ŽŒŽ‘’œ ‘“ŒŒ …Ž•Ž„ˆŒ›• Š ‚…‘…ˆž Ž Š…„ˆ’›Œ „ŽƒŽ‚Ž€Œ € ‹€Ž‚“ž „€’“ " plan_date FORMAT "99/99/9999" skip(2).

for each tmprecid, first loan where recid(loan) eq tmprecid.id NO-LOCK:
fl = false.
    IF AVAIL loan THEN DO:
	FIND FIRST loan-acct of loan WHERE loan-acct.acct-type eq "Šà¥¤ áç" NO-LOCK NO-ERROR.
	Date = STRING(loan-acct.since).
        FIND FIRST loan-acct of loan WHERE loan-acct.acct-type eq "Šà¥¤ áç" AND STRING(loan-acct.since) NE Date  NO-LOCK NO-ERROR.        

        IF AVAIL loan-acct THEN DO:
	    
	END.
	ELSE DO:
	    FIND FIRST loan-acct of loan WHERE loan-acct.acct-type eq "Šà¥¤ áç" NO-LOCK NO-ERROR.
	END.

        rpt_str = STRING(loan.cont-code,"x(12)") + "  ".
        rpt_str2 = STRING(loan.branch-id,"x(4)") + "  ".
	
        IF loan.cust-cat EQ "ž" THEN DO:
	    FIND FIRST cust-corp WHERE cust-corp.cust-id eq loan.cust-id NO-LOCK NO-ERROR.
	    IF AVAIL cust-corp THEN rpt_str = rpt_str + " " + STRING(cust-corp.name-short, "x(50)").
	    ELSE rpt_str = rpt_str + " ----ª«¨¥­â  ­¥âã----".	
	END.
	ELSE IF loan.cust-cat EQ "—" THEN DO:
	    FIND FIRST person WHERE person.person-id eq loan.cust-id NO-LOCK NO-ERROR.
            
            IF AVAIL person THEN  mStrTMP = GetXAttrValueEx("person", STRING(person.person-id), "branch-id", "").

	    IF AVAIL person THEN rpt_str =rpt_Str2 + "@" + rpt_str + " " + STRING(person.name-last + " " + person.first-names + "@"+ trim(replace(person.address[1],',',' ')) + "@" + trim(replace(person.address[2],',',' ')) + "@" + person.phone[1] + "@",'x(200)').
	    ELSE rpt_str = rpt_str + " ----ª«¨¥­â  ­¥âã----".	
	END.
	ELSE DO:
	    rpt_str = rpt_str + " ----ª«¨¥­â  ­¥âã----".	
	END.

        IF AVAIL loan-acct THEN DO:
	    rpt_str = rpt_str + " ‘ç¥â " + STRING(loan-acct.acct, "x(20)").
	END.
	ELSE DO:
	    rpt_str = rpt_str + " ‘ç¥â --------------------".
	END.

        vTotal = 0.	
        loanid = loan.contract + "," + loan.cont-code. 
        FOR EACH term-obl of loan WHERE
               term-obl.contract  = loan.Contract
           AND term-obl.cont-code = loan.Cont-Code
           AND term-obl.idnt      = 1
           AND term-obl.end-date >= loan.open-date
           AND term-obl.end-date <= plan_date
         NO-LOCK:
            RUN summ-t1.p (OUTPUT vSumma,RECID(term-obl),RECID(loan)).
            vTotal = vTotal + vSumma.
        END.
        summ_proc = vTotal.	
        rpt_str = rpt_str + " à®æ¥­âë " + STRING(summ_proc, ">>>,>>>,>>9.99").

        vTotal = 0.
        FOR EACH term-obl of loan WHERE
               term-obl.contract  = loan.Contract
           AND term-obl.cont-code = loan.Cont-Code
           AND term-obl.idnt      = 3
           AND term-obl.end-date >= loan.open-date
           AND term-obl.end-date <= plan_date
        NO-LOCK:
           RUN summ-t.p (OUTPUT vSumma,
                         loan.Contract,
                         loan.Cont-Code,
                         RECID(term-obl),
                         plan_date).
           vTotal = vTotal + vSumma.
        END.
	rpt_str = rpt_str + " Žá­.¤®«£ " + STRING(vTotal, ">>>,>>>,>>9.99").	
        summ_osn = vTotal.	
        rpt_str = rpt_str + " Š ¢­¥á¥­¨î " + STRING(summ_osn + summ_proc, ">>>,>>>,>>9.99").
        in-acct = loan-acct.acct.
        in-currency = loan-acct.currency.
        stat = "”".
	in-date = plan_date.
RUN acct-pos IN h_base (in-acct, in-currency, in-date, in-date ,stat).
	inost = abs(IF in-currency = "" THEN sh-bal ELSE sh-val).
	IF inost GE (summ_osn + summ_proc) THEN DO:
	    rpt_str = rpt_str + "   áç¥â¥ " + STRING(inost, ">>>,>>>,>>9.99") + " Ok".
            FOR EACH term-obl of loan 
            WHERE (term-obl.end-date ge plan_date + 1) AND (term-obl.idnt = 3)
            BY term-obl.end-date:
	       if inost > 0 then DO:
               IF inost - (summ_osn + summ_proc) ge term-obl.amt-rub THEN DO:
	           rpt_str = rpt_str + CHR(10) + "                  ‚®§¬. ¤®áà. ¯®£. ®¡ï§. ­  ¤ âã " + STRING(term-obl.end-date, "99/99/9999") + " ‘ã¬¬  " + STRING(term-obl.amt-rub, ">>>,>>>,>>9.99").
	           inost = inost - term-obl.amt-rub.
	       END.
               END.
	    END.
	END.
	ELSE DO:
	    rpt_str = rpt_str + "   áç¥â¥ " + STRING(inost, ">>>,>>>,>>9.99") + " !!".
	    fl = true.
	END.
        if ((summ_osn + summ_proc) NE 0) THEN PUT STREAM slog UNFORMATTED rpt_str SKIP.
    END.
    ELSE DO:
        PUT STREAM slog UNFORMATTED "„®£®¢®à ­¥ ­ ©¤¥­. Ž¡à ¡®âª  ¯à¥à¢ ­ ." SKIP.
    END.
END.

{preview.i &stream="stream slog"}

