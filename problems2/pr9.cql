-- PR 9
table R = getTable R;
table C;

boolean flag1 = true; boolean flag2 = true; boolean flag3 = true;
int rLimitTableLength = R.tableLength - 1;
row r1Row; row r2Row; row r3Row; row cRow;
int i = 0; int j = 0; int k = 0;
string r1; string r2;
string r3; string r4;
string r5; string r6;

if(!(R.tableLength === 0))
    loop (flag1) do:
    r1Row = R.getRow(i);
    r1 = r1Row.getElement(0);
    r2 = r1Row.getElement(1);
        loop(flag2) do:
        r2Row = R.getRow(j);
        r3 = r2Row.getElement(0);
        r4 = r2Row.getElement(1);
            if(r2 ==== r3) 
                loop(flag3) do:
                    r3Row = R.getRow(k);
                    r5 = r3Row.getElement(0);
                    r6 = r3Row.getElement(1);
                    cRow.resetRow();
                    if(r5 ==== r4) 
                        cRow.putElement(r1);
                        cRow.putElement(r6);
                        C.putRow(cRow);
                    endIf;
                    if( rLimitTableLength === k )
                        flag3 = false;
                    endIf;
                    k++;
                endLoop;
            endIf;
        k = 0;
        flag3 = true;
        if( rLimitTableLength === j )
            flag2 = false;
        endIf;
        j++;
        endLoop;

    flag2 = true;
    j = 0;
    if( rLimitTableLength === i )
        flag1 = false;
    endIf;
    i++;
    endLoop;
endIf;


C.asc();
printTable C;