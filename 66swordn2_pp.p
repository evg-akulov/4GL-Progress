/*               Банковская интегрированная система БИСквит
    Copyright: (C) 1992-2009 ТОО "Банковские информационные системы"
     Filename: swordn2.p
      Comment: Печать сводного мемориального ордера в рублях.
               Подключается в CTRL-G в док-тах дня.
   Parameters:
         Uses: 
      Used by:
      Created: 17/11/2009 MUTA 0117598: БМ.печатная форма Сводного мемориального ордера по документам дня (в рублях РФ)
     Modified: 
*/

&GLOB rshb              YES
&GLOB FILE_SWORD_P      YES
&GLOB FILE_SWORD_I_RUB  YES
&GLOB SORT-BY           BY SUBSTRING(op-entry.acct-db, 17, 2) BY op-entry.amt-rub

{66sword.i}
