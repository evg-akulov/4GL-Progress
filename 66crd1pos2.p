/*
               Å†≠™Æ¢·™†Ô ®≠‚•£‡®‡Æ¢†≠≠†Ô ·®·‚•¨† Åàë™¢®‚
    Copyright: (C) 1992-1998 íéé "Å†≠™Æ¢·™®• ®≠‰Æ‡¨†Ê®Æ≠≠Î• ·®·‚•¨Î"
     Filename: crd1pos2.p
      Comment: ¢•§Æ¨Æ·‚Ï Æ·‚†‚™Æ¢ ØÆ ™†‡‚Æ‚•™•
   Parameters:
         Uses:
      Used by:
      Created: 19.03.2004 AVAL 13410
     Modified: 30/08/2011 kraw (0142515) è†‡†¨•‚‡Î NoZeroPos, ColBlk
*/
{globals.i}
{sh-defs.i}
{kautools.lib}
{parsin.def}
{intrface.get blkob}

DEFINE INPUT PARAMETER iParam AS CHARACTER NO-UNDO.

DEFINE VARIABLE mKauID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInCurrency AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctCat    AS CHARACTER NO-UNDO INITIAL "o".
DEFINE VARIABLE mBSum       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mOSum       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE mUserList   AS CHARACTER NO-UNDO.
DEFINE VARIABLE mAcctName   AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE mNumClient  AS INT64     NO-UNDO.

DEFINE VARIABLE mNoZeroPos AS LOGICAL    NO-UNDO.
DEFINE VARIABLE mColBlk    AS LOGICAL    NO-UNDO.

DEFINE BUFFER xacct FOR acct.

DEFINE TEMP-TABLE ttClient
   FIELD cid   AS INT64
   FIELD cname AS CHARACTER 
   INDEX indx1 IS UNIQUE cname 
   INDEX indx2 IS UNIQUE cid
.
DEFINE TEMP-TABLE ttRes 
   FIELD cid   AS INT64
   FIELD oacct AS CHARACTER 
   FIELD bacct AS CHARACTER 
   FIELD osum  AS DECIMAL   
   FIELD bsum  AS DECIMAL   
   FIELD fBlk  AS LOGICAL
   INDEX indx1 IS UNIQUE cid oacct bacct
.
DEFINE STREAM out-doc.
/*---------------------îìçäñàà------------------------------------------------*/
FUNCTION PrintHeader RETURNS CHAR 
:
   PUT
      STREAM out-doc UNFORMATTED
      "    ÇÖÑéåéëíú éëíÄíäéÇ èé ëóÖíÄå äÄêíéíÖäà 90901    " 
      SKIP
      "Ñãü éèãÄíõ äéíéêõï Öëíú ëêÖÑëíÇÄ çÄ êÄëóÖíçõï ëóÖíÄï" 
      SKIP
      "                   áÄ " end-date 
      SKIP
      "⁄" FILL("ƒ",70) 
      "¬" FILL("ƒ",25) 
      "¬" FILL("ƒ",25) 
      "¬" FILL("ƒ",19) 
      "¬" FILL("ƒ",19) 
   .

   IF mColBlk THEN 
   DO:
      PUT
         STREAM out-doc UNFORMATTED
         "¬" FILL("ƒ",10) 
      .
   END.

   PUT
      STREAM out-doc UNFORMATTED
      "ø" 
      SKIP
      "≥" "ç†®¨•≠Æ¢†≠®• ™´®•≠‚†" FORMAT "x(70)"
      "≥" "Ç≠•°†´†≠·Æ¢Î© ·Á•‚"   FORMAT "x(25)"
      "≥" "ê†·Á•‚≠Î© ·Á•‚"       FORMAT "x(25)"
      "≥" "é·‚†‚Æ™ ØÆ ·Á 90901"  FORMAT "x(19)"
      "≥" "é·‚†‚Æ™ ≠† ‡/· "      FORMAT "x(19)"
   .

   IF mColBlk THEN 
   DO:
      PUT
         STREAM out-doc UNFORMATTED
         "≥Å´Æ™®‡Æ¢™†"
      .
   END.

   PUT
      STREAM out-doc UNFORMATTED
      "≥" 
      SKIP
      "√" FILL("ƒ",70) 
      "≈" FILL("ƒ",25) 
      "≈" FILL("ƒ",25) 
      "≈" FILL("ƒ",19) 
      "≈" FILL("ƒ",19) 
   .

   IF mColBlk THEN 
   DO:
      PUT
         STREAM out-doc UNFORMATTED
         "≈" FILL("ƒ",10) 
      .
   END.

   PUT
      STREAM out-doc UNFORMATTED
      "¥" 
      SKIP
   .
   RETURN "".
END FUNCTION.
/*----------------------------------------------------------------------------*/
FUNCTION PrintFooter RETURNS CHARACTER 
:
   PUT
      STREAM out-doc UNFORMATTED
      "¿" FILL("ƒ",70) 
      "¡" FILL("ƒ",25) 
      "¡" FILL("ƒ",25) 
      "¡" FILL("ƒ",19) 
      "¡" FILL("ƒ",19) 
   .

   IF mColBlk THEN 
   DO:
      PUT
         STREAM out-doc UNFORMATTED
         "¡" FILL("ƒ",10) 
      .
   END.

   PUT
      STREAM out-doc UNFORMATTED
      "Ÿ" 
      SKIP
   .
   {signatur.i 
      &department = branch 
      &stream     = "STREAM out-doc"
      &user-only  = "YES"
   }
   RETURN "".
END FUNCTION.
/*----------------------------------------------------------------------------*/
FUNCTION PrintStrReport RETURNS CHAR 
   ( 
   INPUT iName  AS CHARACTER,
   INPUT iOAcct AS CHARACTER,
   INPUT iBAcct AS CHARACTER,
   INPUT iOSum  AS DECIMAL,
   INPUT iBSum  AS DECIMAL
   ) 
:

   IF iOSum EQ 0.0 AND mNoZeroPos THEN
   RETURN "".

   PUT
      STREAM out-doc UNFORMATTED
      "≥" iName  FORMAT "x(70)"
      "≥" iOAcct FORMAT "x(25)"
      "≥" iBAcct FORMAT "x(25)"
      "≥" iOSum  FORMAT "->>>,>>>,>>>,>>9.99" 
      "≥" iBSum  FORMAT "->>>,>>>,>>>,>>9.99"
   .

   IF mColBlk THEN 
   DO:
      PUT
         STREAM out-doc UNFORMATTED
         "≥"
         IF ttRes.fBlk THEN "Å´Æ™®‡Æ¢†≠"
                       ELSE "          "
      .
   END.

   PUT
      STREAM out-doc UNFORMATTED
      "≥" 
      SKIP
   .
END FUNCTION.
/*----------------------------------------------------------------------------*/
FUNCTION getAcctName RETURNS CHARACTER
   ( INPUT iAcct AS CHARACTER,
     INPUT iCurr AS CHARACTER
   ) 
:
   DEFINE VARIABLE vAcctName AS CHARACTER NO-UNDO EXTENT 2.
   DEFINE BUFFER acct FOR acct.
   FIND FIRST acct WHERE 
              acct.acct     EQ iAcct
          AND acct.currency EQ iCurr
              NO-LOCK NO-ERROR.
   IF AVAILABLE acct THEN
   DO:
      {getcust.i                                            
         &name   = "vAcctName"                              
         &OFFinn = '/*'                                     
      }                                                      
      vAcctName[1] = TRIM(vAcctName[1] + " " + vAcctName[2]). 
   END.
   RETURN vAcctName[1].
      
END FUNCTION.
/*----------------------------------------------------------------------------*/
FUNCTION getNumClient RETURNS INT64
   ( INPUT iCName AS CHARACTER
   ) 
:
   FIND FIRST ttClient WHERE 
              ttClient.cname EQ iCName 
              NO-LOCK NO-ERROR.
   IF NOT AVAIL ttClient THEN
   DO:   
      mNumClient = mNumClient + 1.
      CREATE ttClient.
             ttClient.cid   = mNumClient.
             ttClient.cname = iCName .
      RETURN mNumClient.
   END.
   ELSE 
      RETURN ttClient.cid.

END FUNCTION.
/*----------------------------------------------------------------------------*/
FUNCTION addRecTT RETURNS CHAR 
   ( INPUT iCID   AS INT64,
     INPUT iBAcct AS CHARACTER, 
     INPUT iOAcct AS CHARACTER, 
     INPUT iBSum  AS DECIMAL, 
     INPUT iOSum  AS DECIMAL 
   ) 
:
   DEFINE BUFFER xacct FOR acct.

   CREATE ttRes.
          ttRes.cid   = iCID.
          ttRes.bacct = iBAcct.
          ttRes.oacct = iOAcct.
          ttRes.bsum  = iBSum .
          ttRes.osum  = iOSum .
          ttRes.fBlk  = NO.

   IF mColBlk THEN 
   DO:
      FIND FIRST xacct WHERE xacct.acct EQ iBAcct NO-LOCK NO-ERROR.

      IF AVAILABLE xacct THEN
      DO:
         ttRes.fBlk  = BlockAcct(iBAcct + "," + xacct.currency,
                                 DATETIME(end-date + 1) - 1) NE "".
      END.
   END.
END FUNCTION.
/*---------------------äéçÖñ îìçäñàà------------------------------------------*/
/*------------------------------ÇÇéÑ çÄëíêéÖä---------------------------------*/

mNoZeroPos = GetParamByNameAsChar(iParam, "NoZeroPos", "") NE "".
mColBlk    = GetParamByNameAsChar(iParam, "ColBlk",    "") NE "".

PAUSE 0.
UPDATE mInCurrency   LABEL "ÇÄãûíÄ"
       end-date      LABEL "ÑÄíÄ êÄëóÖíÄ"
WITH FRAME enter-cond
     WIDTH 40
     SIDE-LABELS
     CENTERED
     ROW 10
     TITLE "[ ç†Á†´Ï≠Î• „·´Æ¢®Ô ]"
     OVERLAY
EDITING:
       READKEY.
       IF     (   KEYLABEL(LASTKEY) EQ "F1"
               OR KEYLABEL(LASTKEY) EQ "F10")
          AND FRAME-FIELD EQ "END-DATE" THEN 
       DO:
          RUN calend.p.
          IF (LASTKEY EQ 13 OR LASTKEY EQ 10) AND pick-value <> ? THEN 
          DO:
            FRAME-VALUE = pick-value.
            ASSIGN end-date.
            DISPLAY end-date WITH FRAME enter-cond.
          END.
       END.
       ELSE IF KEYLABEL(LASTKEY) EQ "F1" AND FRAME-FIELD EQ "mInCurrency" THEN 
       DO TRANS:
          RUN cur-code.p ("currency", 10).
          IF (LASTKEY EQ 13 OR LASTKEY EQ 10) AND pick-value <> ? THEN
             DISPLAY pick-value @ mInCurrency WITH FRAME enter-cond.
       END.
       ELSE
          APPLY LASTKEY.
END.
HIDE FRAME enter-cond.
/*------------------------äéçÖñ ÇÇéÑÄ çÄëíêéÖä--------------------------------*/
/*------------------------ëéëíÄÇãÖçàÖ ëèàëäÄ èéãúáéÇÄíÖãÖâ--------------------*/
rights-pos  = getThisUserXAttrValue("è‡Æ·¨Æ‚‡é·‚")  EQ "Ñ†".
rights-user = getThisUserXAttrValue("è‡Æ·¨Æ‚‡ëÆ‚‡") EQ "Ñ†".

IF rights-user THEN 
DO:
   userids = getSlaves().
   IF NOT CAN-DO(userids, userid("bisquit")) THEN 
   DO:
      {additem.i userids userid(""bisquit"")}
   END.
   IF userids NE '*' THEN 
   DO:
      mUserList = "".
      FOR EACH _user WHERE 
                CAN-DO(userids, _user._userid)
                NO-LOCK
      :
         {additem.i mUserList _user._userid}
      END.
   END.
   ELSE 
      mUserList = userids.
END.
ELSE 
   mUserList = "*".
/*------------------------äéçÖñ ëéëíÄÇãÖçàÖ ëèàëäÄ èéãúáéÇÄíÖãÖâ--------------*/
/*-------------------åÄäêéë, àëèéãúáìÖåõâ Ñãü ÅÄã.ëóÖíéÇ ® ãàñ.ëóÖíéÇ---------*/
mKauID    = "ä†‡‚-™†1".
&SCOP PRINT-KAU1 RUN fdbacct ( BUFFER acct,                                ~
                               "Ñ†",                                       ~
                               mKauID                                    ~
                             ).                                            ~
                 FIND FIRST ttKau WHERE                                    ~
                            ttKau.ftbName EQ "ACCTB"                       ~
                            NO-LOCK NO-ERROR.                              ~
                 RELEASE xacct.                                            ~
                 IF AVAILABLE(ttKau) THEN                                  ~
                    FIND FIRST xacct WHERE                                 ~
                               RECID(xacct) EQ ttKau.fRecId                ~
                               NO-LOCK NO-ERROR.                           ~
                 IF NOT AVAILABLE(xacct) THEN                              ~
                    FIND FIRST xacct WHERE                                 ~
                               xacct.acct-cat EQ "b"                       ~
                           AND CAN-DO(mAcctContCrd2,xacct.contract)         ~
                           AND xacct.currency EQ acct.currency             ~
                           AND xacct.cust-cat EQ acct.cust-cat             ~
                           AND xacct.cust-id  EQ acct.cust-id              ~
                               NO-LOCK NO-ERROR.                           ~
                 RUN acct-pos IN h_base ( acct.acct,                       ~
                                          acct.currency,                   ~
                                          end-date,                        ~
                                          end-date,                        ~
                                          ?                                ~
                                        ).                                 ~
                 mOSum = IF acct.currency EQ "" THEN sh-bal                ~
                                                ELSE sh-val.               ~
                 IF acct.side EQ "è" THEN                                  ~
                    mOSum = 0 - mOSum.                                     ~
                 ASSIGN mBSum = 0                                          ~
                        mAcctName[1] = "".                                 ~
                 IF AVAILABLE xacct THEN                                   ~
                 DO:                                                       ~
                    RUN acct-pos IN h_base ( xacct.acct,                   ~
                                             xacct.currency,               ~
                                             end-date,                     ~
                                             end-date,                     ~
                                             ?                             ~
                                           ).                              ~
                    mBSum = IF xacct.currency EQ "" THEN sh-bal            ~
                                                    ELSE sh-val.           ~
                    IF xacct.side EQ "è" THEN                              ~
                       mBSum = 0 - mBSum.                                  ~
                    mAcctName[1] = getAcctName(xacct.acct,xacct.currency). ~
                 END.                                                      ~
&SCOP PRINT-KAU2 addRecTT ( getNumClient( mAcctName[1] ),                  ~
                            IF AVAILABLE xacct THEN xacct.acct ELSE "-" ,  ~
                            acct.acct,                                     ~
                            mBSum,                                         ~
                            mOSum                                          ~
                          ).                                               ~
/*-----------äéçÖñ   åÄäêéë, àëèéãúáìÖåõâ Ñãü ÅÄã.ëóÖíéÇ ® ãàñ.ëóÖíéÇ---------*/
/*------------------------ëÅéê àçîéêåÄñàà-------------------------------------*/
{empty ttClient}    
{empty ttRes}    
FOR EACH bal-acct WHERE 
         bal-acct.kau-id EQ mKauID 
         NO-LOCK,
    EACH acct OF bal-acct WHERE 
         (   acct.kau-id EQ ""
          OR acct.kau-id EQ ? )
     AND CAN-DO(mUserList,acct.user-id)
     AND acct.open-date LE end-date
     AND (   acct.close-date EQ ? 
          OR acct.close-date GE end-date )
     AND CAN-DO(mInCurrency,acct.currency)
     AND CAN-DO(shFilial, acct.filial-id) 	 
         NO-LOCK
   BREAK BY bal-acct.bal-acct
         BY acct.currency
         BY SUBSTRING( TRIM(acct.acct),
                       LENGTH(TRIM(acct.acct)) - 7 )
:
   {&PRINT-KAU1}
   {&PRINT-KAU2}
END.
FOR EACH acct WHERE 
         acct.kau-id    EQ mKauID
     AND acct.acct-cat  EQ mAcctCat
     AND CAN-DO(mUserList,acct.user-id)
     AND acct.open-date LE end-date 
     AND CAN-DO(mInCurrency,acct.currency)
     AND CAN-DO(shFilial, acct.filial-id) 	 	 
         NO-LOCK
   BREAK BY acct.currency
         BY SUBSTRING(TRIM(acct.acct),
                      LENGTH(TRIM(acct.acct)) - 7)
:
   {&PRINT-KAU1}
   {&PRINT-KAU2}
END.
/*----------------èÖóÄíú------------------------------------------------------*/
{setdest.i 
    &stream = "STREAM out-doc" 
    &cols   = 50
}
PrintHeader().
FOR EACH ttClient
         NO-LOCK ,
    EACH ttRes OF ttClient
         NO-LOCK 
   BREAK BY ttClient.cname
:
   IF ttRes.bsum GT 0 THEN
      PrintStrReport ( ttClient.cname,
                       ttRes.oacct   ,
                       ttRes.bacct   ,
                       ttRes.osum    ,
                       ttRes.bsum     
                     ).
END.
PrintFooter().
{preview.i  
    &stream = "STREAM out-doc"
}
/*------------------------äéçÖñ èÖóÄíà----------------------------------------*/

{intrface.del}          /* ÇÎ£‡„ß™† ®≠·‚‡„¨•≠‚†‡®Ô. */ 

