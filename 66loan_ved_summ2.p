{globals.i}
{sh-defs.i}
{tmprecid.def}

DEF VAR loanid    AS CHAR NO-UNDO.
DEF VAR plan_date AS DATE NO-UNDO.
DEF VAR summ_proc AS DEC  NO-UNDO.
DEF VAR summ_osn  AS DEC  NO-UNDO.
DEF VAR vTotal  AS DEC  NO-UNDO.
DEF VAR vSumma  AS DEC  NO-UNDO.
DEF VAR rpt_str   AS CHAR NO-UNDO.
DEF VAR in-acct     LIKE acct.acct     NO-UNDO.
DEF VAR in-currency LIKE acct.currency NO-UNDO.
DEF VAR in-date     AS   DATE          NO-UNDO.
DEF VAR inost as dec no-undo.
DEF VAR stat as char no-undo.
DEF VAR fl as logical no-undo.
DEF BUFFER term-obl1 FOR term-obl.

DEF new global shared STREAM slog.

{setdest.i &stream="stream slog" &file="loan_ved_summ.log"}

MESSAGE "������ �������� ���� " UPDATE plan_date.

PUT STREAM slog UNFORMATTED " 			��������� ���� ����������� � �������� �� ��������� ��������� �� �������� ���� " plan_date FORMAT "99/99/9999" skip(2).

for each tmprecid, first loan where recid(loan) eq tmprecid.id NO-LOCK:
fl = false.
    IF AVAIL loan THEN DO:
	FIND FIRST loan-acct of loan WHERE loan-acct.acct-type eq "�।����" NO-LOCK NO-ERROR.
        rpt_str = STRING(loan.cont-code,"x(12)") + "  ".
	IF loan.cust-cat EQ "�" THEN DO:
	    FIND FIRST cust-corp WHERE cust-corp.cust-id eq loan.cust-id NO-LOCK NO-ERROR.
	    IF AVAIL cust-corp THEN rpt_str = rpt_str + " " + STRING(cust-corp.name-short, "x(20)").
	    ELSE rpt_str = rpt_str + " ----������ ����----".	
	END.
	ELSE IF loan.cust-cat EQ "�" THEN DO:
	    FIND FIRST person WHERE person.person-id eq loan.cust-id NO-LOCK NO-ERROR.
	    IF AVAIL person THEN rpt_str = rpt_str + " " + STRING(person.name-last + " " + person.first-names, "x(20)").
	    ELSE rpt_str = rpt_str + " ----������ ����----".	
	END.
	ELSE DO:
	    rpt_str = rpt_str + " ----������ ����----".	
	END.

        IF AVAIL loan-acct THEN DO:
	    rpt_str = rpt_str + " ��� " + STRING(loan-acct.acct, "x(20)").
	END.
	ELSE DO:
	    rpt_str = rpt_str + " ��� --------------------".
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
        rpt_str = rpt_str + " ��業�� " + STRING(summ_proc, ">>>,>>>,>>9.99").

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
	rpt_str = rpt_str + " ��.���� " + STRING(vTotal, ">>>,>>>,>>9.99").	
        summ_osn = vTotal.	
        rpt_str = rpt_str + " � ���ᥭ�� " + STRING(summ_osn + summ_proc, ">>>,>>>,>>9.99").
        in-acct = loan-acct.acct.
        in-currency = loan-acct.currency.
        stat = "�".
	in-date = plan_date.
RUN acct-pos IN h_base (in-acct, in-currency, in-date, in-date ,stat).
	inost = abs(IF in-currency = "" THEN sh-bal ELSE sh-val).
	IF inost GE (summ_osn + summ_proc) THEN DO:
	    rpt_str = rpt_str + " �� ��� " + STRING(inost, ">>>,>>>,>>9.99") + " Ok".
            FOR EACH term-obl of loan 
            WHERE (term-obl.end-date ge plan_date + 1) AND (term-obl.idnt = 3)
            BY term-obl.end-date:
	       if inost > 0 then DO:
	       vTotal = 0.
               FOR EACH term-obl1 of loan WHERE
                     term-obl1.contract  = loan.Contract
                 AND term-obl1.cont-code = loan.Cont-Code
                 AND term-obl1.idnt      = 3
                 AND term-obl1.end-date >= loan.open-date
                 AND term-obl1.end-date <= term-obl.end-date
               NO-LOCK:
                  RUN summ-t.p (OUTPUT vSumma,
                                loan.Contract,
                                loan.Cont-Code,
                                RECID(term-obl1),
                                term-obl.end-date).
                  vTotal = vTotal + vSumma.
               END.
               IF inost - (summ_osn + summ_proc) ge term-obl.amt-rub THEN DO:
	           rpt_str = rpt_str + CHR(10) + "                  ����. ����. ���. ���. �� ���� " + STRING(term-obl.end-date, "99/99/9999") + " �㬬� " + STRING(term-obl.amt-rub, ">>>,>>>,>>9.99").
	           inost = inost - term-obl.amt-rub.
	       END.
               END.
	    END.
	END.
	ELSE DO:
	    rpt_str = rpt_str + " �� ��� " + STRING(inost, ">>>,>>>,>>9.99") + " !!".
	    fl = true.
	END.
        if ((summ_osn + summ_proc) NE 0) THEN PUT STREAM slog UNFORMATTED rpt_str SKIP.
    END.
    ELSE DO:
        PUT STREAM slog UNFORMATTED "������� �� ������. ��ࠡ�⪠ ��ࢠ��." SKIP.
    END.
END.

{preview.i &stream="stream slog"}

