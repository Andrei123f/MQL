-- PR 6
table P = getTable P;
table Q = getTable Q;
table R = getTable R;
table S = getTable S;
table T = getTable T;
table C;
boolean flag1 = true; boolean flag2 = true; boolean flag3 = true; boolean flag4 = true; boolean flag5 = true;
string a;
row pRow; row qRow; row rRow; row sRow; row tRow; row cRow;
int iP = 0; int iQ = 0; int iR = 0; int iS = 0; int iT = 0;
int pLimitTableLength = P.tableLength - 1;
int qLimitTableLength = Q.tableLength - 1;
int rLimitTableLength = R.tableLength - 1;
int sLimitTableLength = S.tableLength - 1;
int tLimitTableLength = T.tableLength - 1;
if(!(P.tableLength === 0))
    loop(flag1) do:
        pRow = P.getRow(iP);
            loop(flag2) do:
                qRow = Q.getRow(iQ);
                loop(flag3) do:
                    rRow = R.getRow(iR);
                    loop(flag4) do:
                        sRow = S.getRow(iS);
                        loop(flag5) do:
                            tRow = T.getRow(iT);
                            cRow.resetRow();
                            cRow.mergeRow(pRow);
                            cRow.mergeRow(qRow);
                            cRow.mergeRow(rRow);
                            cRow.mergeRow(sRow);
                            cRow.mergeRow(tRow);
                            C.putRow(cRow);
                            if(tLimitTableLength === iT)
                                flag5 = false;
                            endIf; 
                        endLoop;
                        iT = 0;
                        flag5 = true;
                        if(sLimitTableLength === iS)
                            flag4 = false;
                        endIf;
                    endLoop;
                    iS = 0;
                    flag4 = true;
                    if(rLimitTableLength === iR)
                        flag3 = false;
                    endIf;
                endLoop;
                flag3 = true;
                iR = 0;
                if(qLimitTableLength === iQ)
                    flag2 = false;
                 endIf;
                 iQ++;
            endLoop;
        flag2 = true;
        iQ = 0;
        if(pLimitTableLength === iP)
            flag1 = false;
        endIf;
        iP++;
    endLoop;
endIf;

C.asc();
printTable C;