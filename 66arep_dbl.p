DEFINE BUFFER bop-entry FOR op-entry.
DEFINE BUFFER fop-entry FOR op-entry.
DEF BUFFER fop FOR op.
DEFINE VAR isDbl AS LOGICAL NO-UNDO INIT NO.



for each fop-entry WHERE 
   (fop-entry.op-date >= TODAY - 3) AND
   (fop-entry.op-date <= TODAY) AND
   CAN-DO('40*',fop-entry.acct-db) AND
   CAN-Do('30102*,30301*,40*',fop-entry.acct-cr) NO-LOCK,
FIRST fop WHERE fop.op = fop-entry.op NO-LOCK,   

EACH op-entry WHERE 
    (op-entry.op-date >= (fop-entry.op-date - 3) AND
     op-entry.op-date <= fop-entry.op-date) AND
    op-entry.op <> fop-entry.op AND
    op-entry.acct-cr = fop-entry.acct-cr AND
    op-entry.acct-db = fop-entry.acct-db AND
    op-entry.amt-rub = fop-entry.amt-rub NO-LOCK,
FIRST op WHERE op.op = op-entry.op AND op.doc-num = fop.doc-num NO-LOCK: 


display 
        op-entry.op COLUMN-LABEL "‚­.­®¬¥à" 
        op-entry.op-date 
        op-entry.op-status
        fop-entry.op
        fop-entry.op-date
        fop-entry.op-status
   WITH OVERLAY CENTERED ROW 5 
    TITLE "  ‚ˆŒ€ˆ…! …‘’œ •†ˆ… „Š“Œ…’›  " 10 DOWN . 


  isDbl = YES.
END.


IF NOT isDbl THEN

  MESSAGE
    "    ‡€„‚…›• „Š“Œ…’‚ … €‰„…    "
      VIEW-AS ALERT-BOX.

pause. 