/*
               ������᪠� ��⥣�஢����� ��⥬� �������
    Copyright: (C) 1992-2010 ��� "������᪨� ���ଠ樮��� ��⥬�"
     Filename: CLIENTGUA.I
      Comment: 0131666 ����⨢�� ���� �� ������ࠬ ���ᯥ祭�� �뤥������ �����⮢
   Parameters:
         Uses:
      Used by:
      Created: 19.10.2010 13:24 feok    
     Modified: 19.10.2010 13:24 feok    
*/

FOR EACH loan WHERE (loan.contract EQ "�।��"
                  OR loan.contract EQ "�������")
                AND  loan.cust-cat   EQ {1}
                AND  loan.cust-id    EQ {2}
                AND (loan.close-date EQ ?
                 OR  loan.close-date GT vDate)
                AND  loan.open-date  LE vDate
NO-LOCK:
   FOR EACH term-obl WHERE  term-obl.cont-code EQ loan.cont-code 
                       AND  term-obl.contract  EQ loan.contract
                       AND  term-obl.idnt      EQ 5 
                       AND (term-obl.end-date  EQ ?
                        OR  term-obl.end-date  GT vDate) 
                       AND (term-obl.sop-date  EQ ?
                        OR  term-obl.sop-date  GT vDate)
                       AND  term-obl.fop-date  LE vDate
   NO-LOCK:
      
      CREATE ttDog.

      ASSIGN
         mSurr            = term-obl.contract         + "," + 
                            term-obl.cont-code        + "," + 
                            STRING(term-obl.idnt)     + "," + 
                            STRING(term-obl.end-date) + "," + 
                            STRING(term-obl.nn)
         ttDog.DogNum     = IF loan.doc-num = "" THEN loan.cont-code ELSE loan.doc-num
         ttDog.NameZaem   = vTmpStr
         ttDog.FopDate    = term-obl.fop-date 
         ttDog.ObespNum   = ENTRY(1, GetXAttrValueEx ("term-obl", mSurr, "��������", ""), "@")
         ttDog.ObespChar  = GetXAttrValueEx ("term-obl", mSurr, "��������", "") + "," + REPLACE(GetXAttrValueEx ("term-obl", mSurr, "��������", ""),";",":")
         ttDog.KachObesp  = Get_QualityGar ("comm-rate",mSurr, vDate)
         ttDog.Poruch     = GetXAttrValueEx ("term-obl", mSurr, "����⢎�", "")
         ttDog.SumObesp   = term-obl.amt-rub 
         ttDog.ObespCheck = GetXAttrValueEx ("term-obl", mSurr, "��⠏஢��", "")
         ttDog.EqualOb    = GetXAttrValueEx ("term-obl", mSurr, "��ࠢ������", "")
         ttDog.Pz         = GetXAttrValueEx ("term-obl", mSurr, "��᫥�_�����", "")

         ttDog.BKPKat     = GetXAttrValueEx ("loan", string(loan.contract) + "," + string(loan.cont-code),"���ண���","").
         ttDog.FC         = GetXAttrValueEx ("loan", string(loan.contract) + "," + string(loan.cont-code),"�����ண","").
         ttDog.PK         = GetXAttrValueEx ("loan", string(loan.contract) + "," + string(loan.cont-code),"�ண�।","").
      .
   END.
END.
