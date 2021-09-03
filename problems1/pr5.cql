-- PR 5
table A = getTable A;
table C;
boolean flag1 = true;
string a1; string empty;
string p = "0";
row aRow; row cRow;
int i = 0; int aLimitTableLength = A.tableLength - 1;

if(!(A.tableLength === 0))
    loop (flag1) do:
        cRow.resetRow();
        aRow = A.getRow(i);
        a1 = aRow.getElement(0);
        cRow.resetRow();
        cRow.putElement(a1);
        cRow.putElement(p);
        cRow.putElement(a1);
        C.putRow(cRow);
        if(aLimitTableLength === i)
            flag1 = false;
        endIf;
        i++;
    endLoop;
endIf;
C.asc();
printTable C;