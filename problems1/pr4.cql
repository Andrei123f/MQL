-- PR 4
table A = getTable A;
table C;
boolean flag1 = true;
string a1; string a2; string empty;
row aRow; row cRow;
int i = 0; int aLimitTableLength = A.tableLength - 1;

if(!(A.tableLength === 0))
    loop (flag1) do:
        cRow.resetRow();
        aRow = A.getRow(i);
        a1 = aRow.getElement(0);
        a2 = aRow.getElement(1);
        if( ! (a2 ==== empty) )
            cRow.putElement(a1);
            cRow.putElement(a2);
            C.putRow(cRow);
        endIf;

        if(aLimitTableLength === i)
            flag1 = false;
        endIf;
        i++;
    endLoop;
endIf;
C.asc();
printTable C;