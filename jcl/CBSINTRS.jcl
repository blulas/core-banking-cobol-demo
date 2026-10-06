//CBSINTRS JOB (CBS,DEP),'DEPOSIT INT ACCRUAL',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* JOB      : CBSINTRS                                               *
//* PURPOSE  : DAILY DEPOSIT INTEREST ACCRUAL (INTACCR) - EOD STEP 11 *
//*            STAND-ALONE RERUN VERSION OF CBSEOD STEP EOD11.        *
//*            RE-PRICES EVERY INTEREST-BEARING ACTIVE / DORMANT      *
//*            DEPOSIT ACCOUNT VIA RATETIER (RELATIONSHIP BALANCE AND *
//*            SEGMENT FROM CUSTMST), ACCRUES EACH CALENDAR DAY SINCE *
//*            ACCT-LAST-ACCRUAL-DATE VIA INTCALC AND UPDATES THE     *
//*            STATEMENT CYCLE ACCUMULATORS.                          *
//*            MUST RUN AFTER POSTTRAN / SWEEPRUN (USES CLOSING       *
//*            LEDGER BALANCE) AND BEFORE CDMATUR AND INTCAP.         *
//*            RERUN: SAFE - ACCOUNTS WITH LAST-ACCRUAL-DATE = TODAY  *
//*            ARE SKIPPED. NO PARM.                                  *
//* CHANGE LOG                                                        *
//* 1991-03-11 RJT  ORIGINAL                                          *
//* 2008-11-24 DPS  RATECHG REPORT TO GDG (INC10442)                  *
//* 2019-08-19 AKW  CUSTMST ADDED FOR RELATIONSHIP BONUS - CHG1655    *
//*********************************************************************
//JOBLIB   DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//*
//*--------------------------------------------------------------------
//* STEP010 - IMAGE COPY ACCTMST BEFORE UPDATE
//*--------------------------------------------------------------------
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ACCTIN   DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//ACCTOUT  DD DSN=CBS.BKUP.ACCTMST.INTACCR(+1),DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(50,10),RLSE),DCB=(RECFM=VB,LRECL=468)
//SYSIN    DD *
  REPRO INFILE(ACCTIN) OUTFILE(ACCTOUT)
/*
//*
//*--------------------------------------------------------------------
//* STEP020 - INTACCR  DAILY ACCRUAL
//*--------------------------------------------------------------------
//STEP020  EXEC PGM=INTACCR,COND=(0,NE,STEP010)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//PRODMST  DD DSN=CBS.PROD.PRODMST,DISP=SHR
//CUSTMST  DD DSN=CBS.PROD.CUSTMST,DISP=SHR
//RATECHG  DD DSN=CBS.PROD.RPT.INTACCR(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(30,15),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP030 - RATE CHANGE REPORT TO DEPOSIT OPERATIONS / ALCO
//*--------------------------------------------------------------------
//STEP030  EXEC PGM=IEBGENER,COND=(4,LT,STEP020)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=CBS.PROD.RPT.INTACCR(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(R,,DEPR)
//SYSIN    DD DUMMY
//
