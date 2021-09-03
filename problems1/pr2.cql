-- PR 2
table A = getTable A;
table C;
boolean flag1 = true;
boolean flag2 = true;
string a1;
string a2;
string a3;
row aRow;
row cRow;
int i = 0;
int aLimitTableLength = A.tableLength - 1;
if(!(A.tableLength === 0))
    loop (flag1) do:
        aRow = A.getRow(i);
        a1 = aRow.getElement(0);
        a2 = aRow.getElement(1);
        a3 = aRow.getElement(2);
        cRow.resetRow();
        cRow.putElement(a3);
        cRow.putElement(a1);
        if(a1 ==== a2 )
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