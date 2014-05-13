{globals.i}
{setdest.i}
{tmprecid.def}

DEF VAR br AS CHAR NO-UNDO.

for each tmprecid, first person where recid(person) eq tmprecid.id NO-LOCK:

 br = GetXattrValueEx("person", STRING(person.person-id),"branch-id", ?).
 PUT UNFORMATTED string(person.person-id) + ";" + person.name-last + " " + person.first-names ";" br  SKIP.    
     
end. 


PUT UNFORMATTED "" SKIP(2).
PUT UNFORMATTED  string(today,"99/99/9999") " " string(time,"hh:mm:ss") SKIP(1).

{preview.i}

