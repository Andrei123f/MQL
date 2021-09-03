-- PR 10
table S = getTable S;
table T = getTable T;
table C;
boolean flag1 = true; boolean flag2 = true;
row sRow; row tRow; row cRow;
int i = 0; int j = 0;
int sLimitTableLength = S.tableLength - 1;
int tLimitTableLength = T.tableLength - 1;
string s1; string s2; string s3;
string t1; string t2; string t3;

if(!(S.tableLength === 0))
    loop (flag1) do:
    sRow = S.getRow(i);
    s1 = sRow.getElement(0);
    s2 = sRow.getElement(1);
    s3 = sRow.getElement(2);
        loop(flag2) do:
            tRow = T.getRow(j);
            t1 = tRow.getElement(0);
            t2 = tRow.getElement(1);
            t3 = tRow.getElement(2);
            cRow.resetRow();
            if(s1 ==== s2 and t2 ==== t3)
                cRow.putElement(s3);
                cRow.putElement(t1);
                C.putRow(cRow);
            endIf;
        if( tLimitTableLength === j )
            flag2 = false;
        endIf;
        j++;
        endLoop;
    flag2 = true;
    j = 0;
    if( sLimitTableLength === i )
        flag1 = false;
    endIf;
    i++;
    endLoop;
endIf;

C.asc();
printTable C;
