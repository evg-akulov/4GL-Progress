/*
   Красноярский РФ ОАО "Россельхозбанк"
   Кириллов П.Г. 14.03.2011
   
   Печать счета-фактуры
*/
{globals.i}

DEF INPUT PARAM RID AS RecID NO-UNDO.

{strtout3.i &cols=168 &option=Paged}

RUN 66s-fac-prn.p (RID).

{endout3.i &nofooter=yes}
