-- PR 7
table F = getTable F;
table G = getTable G;
table C;
boolean flag1 = true; boolean flag2 = true;
string f1; string f2; string f3; 
string g1; string g2; string g3;
string empty;
row fRow; row gRow; row cRow;
int i = 0; int j = 0;
int fLimitTableLength = F.tableLength - 1; int gLimitTableLength = G.tableLength - 1;

if( ! (F.tableLength === 0))
    loop (flag1) do:
        fRow = F.getRow(i);
        f1 = fRow.getElement(0);
        f2 = fRow.getElement(1);
        f3 = fRow.getElement(2);
        loop (flag2) do:
            gRow = G.getRow(j);
            g1 = gRow.getElement(0);
            g2 = gRow.getElement(1);
            g3 = gRow.getElement(2);
            cRow.resetRow();

            if ( ! ((f2 ==== empty) or (f3 ==== empty)) )
                if ((f2 ==== g1) and (f3 ==== g2))
                    cRow.putElement(f1);
                    cRow.putElement(g3);
                    C.putRow(cRow);
                endIf;
            endIf;

            if(gLimitTableLength === j)
                flag2 = false;
            endIf;
            j++;
        endLoop;
        flag2 = true;
        j = 0;
        if(fLimitTableLength === i)
            flag1 = false;
        endIf;
        i++;
    endLoop;
endIf;
C.asc();
printTable C;