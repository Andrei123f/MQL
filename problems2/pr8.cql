-- PR 8
table P = getTable P;
table Q = getTable Q;
table C;

boolean flag1 = true;
boolean flag2 = true;
string p1; string p2; string p3; string p4;
string q1; string q2; string q3; string q4;
string empty;

row pRow; 
row qRow;
row cRow;
int i = 0; 
int j = 0;
int pLimitTableLength = P.tableLength - 1;
int qLimitTableLength = Q.tableLength - 1;

if(!(P.tableLength === 0))
    loop (flag1) do:
        pRow = P.getRow(i);
        p1 = pRow.getElement(0);
        p2 = pRow.getElement(1);
        p3 = pRow.getElement(2);
        p4 = pRow.getElement(3);

        loop(flag2) do:
                qRow = Q.getRow(j);
                q1 = qRow.getElement(0);
                q2 = qRow.getElement(1);
                q3 = qRow.getElement(2);
                q4 = qRow.getElement(3);
                cRow.resetRow();

                if( p4 ==== q4 )
                    if( q1 ==== empty )
                        cRow.putElement(p1);
                    else
                        cRow.putElement(q1);
                    endIf;

                     if( q2 ==== empty )
                        cRow.putElement(p2);
                    else
                        cRow.putElement(q2);
                    endIf;

                     if( q3 ==== empty )
                        cRow.putElement(p3);
                    else
                        cRow.putElement(q3);
                    endIf;

                    cRow.putElement(p4);
                    C.putRow(cRow);
                endIf;

                if( qLimitTableLength === j )
                    flag2 = false;
                endIf;
                j++;
            endLoop;
        flag2 = true;
        j = 0;
        if(pLimitTableLength === i)
            flag1 = false;
        endIf;
        i++;
    endLoop;
endIf;
C.asc();
printTable C;