-- PR 1
table A = getTable A;
table B = getTable B;
table C;
boolean flag1 = true;
boolean flag2 = true;
string a;
row aRow;
row bRow;
row cRow;
int i = 0;
int j = 0;
int aLimitTableLength = A.tableLength - 1;
int bLimitTableLength = B.tableLength - 1;
if(!(A.tableLength === 0))
    loop (flag1) do:
        aRow = A.getRow(i);
            loop(flag2) do:
                bRow = B.getRow(j);
                cRow.resetRow();
                cRow.mergeRow(aRow);
                cRow.mergeRow(bRow);
                C.putRow(cRow);
                if(bLimitTableLength === j )
                    flag2 = false;
                endIf;
                j++;
            endLoop;
        flag2 = true;
        j = 0;
        if(aLimitTableLength === i)
            flag1 = false;
        endIf;
        i++;
    endLoop;
endIf;
C.asc();
printTable C;