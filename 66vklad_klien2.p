{globals.i}
{setdest.i}
{tmprecid.def}

def var klient as char.


for each tmprecid, first loan where recid(loan) eq tmprecid.id NO-LOCK:
   find person where person.person-id = loan.cust-id no-lock no-error.

/*80 ��ப*/
PUT UNFORMATTED "                                                                     �ਫ������ 13" SKIP.
PUT UNFORMATTED "                                                                     � �ࠢ���� ������ � ������� � ��� <���ᥫ�宧����>" SKIP.
PUT UNFORMATTED "                                                                     ������᪨� ��⮢ � ����� ���ᨩ᪮� �����樨" SKIP.
PUT UNFORMATTED "                                                                     � �����࠭��� ����� �ਤ��᪨� ��栬, �������㠫�� " SKIP.
PUT UNFORMATTED "                                                                     �।�ਭ���⥫� � 䨧��᪨� ��栬, �������騬�� �" SKIP. 
PUT UNFORMATTED "                                                                     ��⠭�������� ��������⥫��⢮� ���ᨩ᪮� �����樨" SKIP.
PUT UNFORMATTED "                                                                     ���浪� ��⭮� �ࠪ⨪��, � 105-�" SKIP.
PUT UNFORMATTED "                                                                     (�ਪ�� ��� <���ᥫ�宧����> �� 10.05.2007 � 136-��)" SKIP.
PUT UNFORMATTED "                                                                     (� ।��樨 �ਪ���� ��� <���ᥫ�宧����> �� 29.12.2011" SKIP. 
PUT UNFORMATTED "                                                                      � 606-��, �� 31.05.2013 � 268-��)" SKIP(2).

klient = person.name-last + " " + person.first-names.

PUT UNFORMATTED "                     ��ଫ���� �� ������ �����" SKIP(1).
PUT UNFORMATTED "�����������������������������������������������������������������������������������������������������������������������Ŀ" SKIP.
PUT UNFORMATTED "�                                                      �" klient FORMAT "x(64)"                                        "�" SKIP.
PUT UNFORMATTED "� <____>_________20___�. � __________________________  �                                                                �" SKIP.
PUT UNFORMATTED "�                                                      �" person.address FORMAT "x(33)"                                "�" SKIP.
PUT UNFORMATTED "�������������������������������������������������������������������������������������������������������������������������" SKIP(2).

PUT UNFORMATTED "� �।�⠢����� ���ଠ樨 � ���㬥�⮢," SKIP.
PUT UNFORMATTED "����室���� ��� ���������� ᢥ����� � ������" SKIP(3).

PUT UNFORMATTED "� ᮮ⢥��⢨� � �᫮��ﬨ ������� ������᪮�� ���, " loan.doc-ref " ࠭�� �����祭���� � ��襩" SKIP.
PUT UNFORMATTED "�࣠����樥�, ���� ����� �ࠢ� �������� ���ࠢ���� ������� ���쬥��� ������ � �।�⠢����� ���ଠ樨 � ���㬥�⮢, ����室���� ��� ����������" SKIP. 
PUT UNFORMATTED "ᢥ����� � ������, �������� � �����, � ������ ��易� �।��⠢���� ⠪�� ���ଠ�� � ���㬥���." SKIP(1).

PUT UNFORMATTED "� �裡 � ��������� ��ᨬ ��� �।��⠢��� � ���� ����室��� ���㬥��� � ���ଠ�� � ���ᥭ�� ��������� � ���������� � ��।�⥫��" SKIP.
PUT UNFORMATTED "���㬥���, ��������� ����, ⥫�䮭�, ८࣠����樨 ��� �������樨 ��襩 �࣠����樨, � ��㣨� ����������, ᯮᮡ��� �������� �� �ᯮ������ �������." SKIP(1).

PUT UNFORMATTED "�।�⠢������ ���� ���ଠ�� �㤥� �ᯮ�짮���� ������ ��� ���������� ᢥ����� � ��襩 �࣠����樨, ᮤ�ঠ���� � ���ଠ樮���� ��⥬�" SKIP.
PUT UNFORMATTED "�����, � ���㬥��� ���� ����饭� � �ਤ��᪮� ���� ��襩 �࣠����樨, ����饥�� � �����." SKIP(4).

PUT UNFORMATTED "_______________________________________          _________________       _____________________" SKIP.
PUT UNFORMATTED "(��������� 㯮�����祭���� ��� �����/               (�������)           (����஢�� ������)" SKIP.
PUT UNFORMATTED "ॣ�����쭮�� 䨫����/ �������⥫쭮�� ���)" SKIP(1).

PUT UNFORMATTED "��. _________" SKIP.
PUT UNFORMATTED "���. _________" SKIP(50).
     
end.


{preview.i}

