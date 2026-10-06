//CBSGL    JOB (CBS,GL),'GENERAL LEDGER EOD',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* JOB      : CBSGL                                                  *
//* PURPOSE  : GENERAL LEDGER END-OF-DAY (EOD STEP 19).               *
//*              STEP020 GLEXTR   TRANPOST -> BALANCED JOURNAL PAIRS  *
//*                               (GLDERIVE), GLJRNL                  *
//*              STEP030 SORT     GLJRNL BY GL ACCOUNT / COST CENTER  *
//*              STEP040 GLTRIAL  POST GLJRNL TO GLBAL, TRIAL BALANCE *
//*                               (DR MUST EQUAL CR ELSE RC 8 + RECN) *
//*              STEP050 GLRECON  DEPOSIT SUB-LEDGER (ACCTMST) VS GL  *
//*                               LIABILITY BALANCES; BREAKS -> RECN  *
//*            MUST RUN AFTER EVERY BALANCE-CHANGING STEP OF THE DAY  *
//*            (POSTTRAN ... MONTH-END STEP 16, RELBAL) AND BEFORE    *
//*            EODCTL END ROLLS TRANPOST INTO TRANHIST.               *
//*            RC 4 FROM GLEXTR (ITEMS TO SUSPENSE 2999) AND GLRECON  *
//*            (BREAKS) DOES NOT STOP THE CYCLE - GL OPERATIONS CLEAR *
//*            THEM NEXT MORNING. RC 8 FROM GLTRIAL STOPS THE JOB.    *
//*            RERUN: RESTORE GLBAL FROM STEP010 BACKUP, RERUN FROM   *
//*            STEP020 (GLJRNL IS RECREATED EACH RUN).                *
//* CHANGE LOG                                                        *
//* 1988-07-01 RJT  ORIGINAL - GL INTERFACE                           *
//* 2006-02-14 BHS  ORACLE GL - COST CENTER IN KEY, SORT STEP ADDED   *
//* 2011-04-12 DPS  CHG1544 GLRECON SUB-LEDGER RECONCILIATION         *
//* 2015-10-02 DPS  GLEXTR READS TRANPOST                             *
//*********************************************************************
//JOBLIB   DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//*
//*--------------------------------------------------------------------
//* STEP010 - BACK UP GLBAL
//*--------------------------------------------------------------------
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//GLBIN    DD DSN=CBS.PROD.GLBAL,DISP=SHR
//GLBOUT   DD DSN=CBS.BKUP.GLBAL(+1),DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(10,5),RLSE),DCB=(RECFM=VB,LRECL=150)
//SYSIN    DD *
  REPRO INFILE(GLBIN) OUTFILE(GLBOUT)
/*
//*
//*--------------------------------------------------------------------
//* STEP020 - GLEXTR  GENERAL LEDGER EXTRACT
//*--------------------------------------------------------------------
//STEP020  EXEC PGM=GLEXTR,COND=(0,NE,STEP010)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//TRANPOST DD DSN=CBS.PROD.TRANPOST.DAILY,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//GLJRNL   DD DSN=&&GLJRNL,DISP=(NEW,PASS,DELETE),
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=141,BLKSIZE=27918)
//GLEXRPT  DD DSN=CBS.PROD.RPT.GLEXTR(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP030 - SORT JOURNAL BY GL ACCOUNT (17,10) + COST CENTER (27,5)
//*           KEEPS GLBAL ACCESS SEQUENTIAL IN GLTRIAL
//*--------------------------------------------------------------------
//STEP030  EXEC PGM=SORT,COND=(4,LT,STEP020)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=&&GLJRNL,DISP=(OLD,DELETE)
//SORTOUT  DD DSN=CBS.PROD.GLJRNL(+1),DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=141,BLKSIZE=27918)
//SYSIN    DD *
  SORT FIELDS=(17,10,CH,A,27,5,CH,A)
/*
//*
//*--------------------------------------------------------------------
//* STEP040 - GLTRIAL  POST TO GLBAL, TRIAL BALANCE
//*           RC 8 = DEBITS NOT EQUAL CREDITS - CALL GL OPERATIONS
//*--------------------------------------------------------------------
//STEP040  EXEC PGM=GLTRIAL,COND=(4,LT,STEP030)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//GLJRNL   DD DSN=CBS.PROD.GLJRNL(+1),DISP=SHR
//GLBAL    DD DSN=CBS.PROD.GLBAL,DISP=SHR
//ALERTS   DD DSN=CBS.PROD.ALERTS.DAILY,DISP=MOD
//GLTBRPT  DD DSN=CBS.PROD.RPT.GLTRIAL(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP050 - GLRECON  SUB-LEDGER TO GL RECONCILIATION
//*--------------------------------------------------------------------
//STEP050  EXEC PGM=GLRECON,COND=(4,LT,STEP040)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//GLBAL    DD DSN=CBS.PROD.GLBAL,DISP=SHR
//ALERTS   DD DSN=CBS.PROD.ALERTS.DAILY,DISP=MOD
//GLRCRPT  DD DSN=CBS.PROD.RPT.GLRECON(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP060 - REPORTS TO GL OPERATIONS / FINANCE (PRINT DEST GLOP)
//*--------------------------------------------------------------------
//STEP060  EXEC PGM=IEBGENER,COND=(8,LT)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=CBS.PROD.RPT.GLEXTR(+1),DISP=SHR
//         DD DSN=CBS.PROD.RPT.GLTRIAL(+1),DISP=SHR
//         DD DSN=CBS.PROD.RPT.GLRECON(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(R,,GLOP)
//SYSIN    DD DUMMY
//
