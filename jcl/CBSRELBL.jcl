//CBSRELBL JOB (CBS,CIF),'RELATIONSHIP BAL',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* JOB      : CBSRELBL                                               *
//* PURPOSE  : RELATIONSHIP BALANCE AGGREGATION (RELBAL), EOD STEP 18.*
//*            RUNS DAILY AFTER ALL BALANCE-CHANGING STEPS (POSTING,  *
//*            SWEEPS, ACCRUAL, MONTH-END CAPITALISATION / FEES /     *
//*            ESCHEAT) AND BEFORE GL EXTRACT. CUST-TOTAL-REL-BALANCE *
//*            IS CONSUMED NEXT DAY BY FEE WAIVERS AND RATE TIERS.    *
//* CHANGE LOG                                                        *
//* 2005-09-12 BHS  ORIGINAL                                          *
//* 2016-11-07 DPS  ADDED SORT OF REPORT BY BALANCE (CHG1342)         *
//*********************************************************************
//JOBLIB   DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//*
//STEP010  EXEC PGM=RELBAL
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//CUSTMST  DD DSN=CBS.PROD.CUSTMST,DISP=SHR
//RELBRPT  DD DSN=&&RELRPT,DISP=(NEW,PASS),
//            SPACE=(TRK,(10,10)),DCB=(RECFM=FB,LRECL=132)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP020 - SORT DETAIL LINES BY RELATIONSHIP BALANCE DESCENDING
//*           (HEADINGS / TOTALS DROPPED - DETAIL LINES START 'C')
//*--------------------------------------------------------------------
//STEP020  EXEC PGM=SORT,COND=(0,NE,STEP010)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=&&RELRPT,DISP=(OLD,DELETE)
//SORTOUT  DD DSN=CBS.PROD.RPT.RELBAL(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(10,10),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSIN    DD *
  INCLUDE COND=(1,1,CH,EQ,C'C')
  SORT FIELDS=(33,19,CH,D,1,10,CH,A)
/*
//
