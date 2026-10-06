//CBSMTHIF JOB (CBS,DEP),'DEP INT/FEE/STMT ME',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* JOB      : CBSMTHIF                                               *
//* PURPOSE  : DEPOSIT MONTH-END - EOD STEP 16 (DOMAIN 03 PROGRAMS):  *
//*              STEP020 INTCAP   CAPITALISE ACCRUED INTEREST (DINT), *
//*                               BACKUP WITHHOLDING 24% (WBWH).      *
//*                               SVST CAPITALISES QUARTERLY.         *
//*              STEP030 FEEASSES MONTHLY MAINTENANCE FEES (WFMN)     *
//*                               VIA FEECALC / FEEWAIVE; RESETS      *
//*                               FEES-MTD.                           *
//*              STEP040 STMTGEN  STATEMENTS (132 PRINT IMAGE) AND    *
//*                               RESET OF CYCLE ACCUMULATORS.        *
//*            ORDER MATTERS: INTEREST MUST BE CAPITALISED AND FEES   *
//*            ASSESSED BEFORE STATEMENTS ARE CUT, AND FEEASSES MUST  *
//*            READ THE CYCLE STATS BEFORE STMTGEN RESETS THEM.       *
//*            IN CBSEOD ANLYSFEE AND POOLCALC (TREASURY) RUN BETWEEN *
//*            FEEASSES AND STMTGEN.                                  *
//*            ALL THREE EXIT RC 0 ON NON MONTH-END DATES. FOR AN     *
//*            OFF-CYCLE RERUN CHANGE THE PARM TO 'FORCE'.            *
//*            RERUN: RESTORE ACCTMST FROM STEP010 AND DELETE TODAY'S *
//*            40000000/50000000-RANGE ITEMS FROM TRANPOST FIRST.     *
//* CHANGE LOG                                                        *
//* 1991-03-11 RJT  ORIGINAL (INTCAP, STMTGEN)                        *
//* 1996-01-08 RJT  FEEASSES ADDED                                    *
//* 2008-11-24 DPS  STATEMENT FILE TO PRINT VENDOR VIA NDM            *
//* 2017-02-08 AKW  TRANHIST GDG FOR FEEASSES DIRECT DEPOSIT CHECK    *
//*********************************************************************
//JOBLIB   DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//*
//*--------------------------------------------------------------------
//* STEP010 - IMAGE COPY ACCTMST BEFORE UPDATE
//*--------------------------------------------------------------------
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ACCTIN   DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//ACCTOUT  DD DSN=CBS.BKUP.ACCTMST.MTHIF(+1),DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(50,10),RLSE),DCB=(RECFM=VB,LRECL=468)
//SYSIN    DD *
  REPRO INFILE(ACCTIN) OUTFILE(ACCTOUT)
/*
//*
//*--------------------------------------------------------------------
//* STEP020 - INTCAP  INTEREST CAPITALISATION
//*--------------------------------------------------------------------
//STEP020  EXEC PGM=INTCAP,COND=(0,NE,STEP010)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//PRODMST  DD DSN=CBS.PROD.PRODMST,DISP=SHR
//TRANPOST DD DSN=CBS.PROD.TRANPOST.DAILY,DISP=MOD
//INTCAPR  DD DSN=CBS.PROD.RPT.INTCAP(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP030 - FEEASSES  MONTHLY MAINTENANCE FEES
//*--------------------------------------------------------------------
//STEP030  EXEC PGM=FEEASSES,COND=(4,LT,STEP020)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//PRODMST  DD DSN=CBS.PROD.PRODMST,DISP=SHR
//CUSTMST  DD DSN=CBS.PROD.CUSTMST,DISP=SHR
//TRANHIST DD DSN=CBS.PROD.TRANHIST(0),DISP=SHR
//TRANPOST DD DSN=CBS.PROD.TRANPOST.DAILY,DISP=MOD
//FEERPT   DD DSN=CBS.PROD.RPT.FEEASSES(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP040 - STMTGEN  CUSTOMER STATEMENTS
//*--------------------------------------------------------------------
//STEP040  EXEC PGM=STMTGEN,COND=(4,LT)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//PRODMST  DD DSN=CBS.PROD.PRODMST,DISP=SHR
//CUSTMST  DD DSN=CBS.PROD.CUSTMST,DISP=SHR
//TRANHIST DD DSN=CBS.PROD.TRANHIST(0),DISP=SHR
//TRANPOST DD DSN=CBS.PROD.TRANPOST.DAILY,DISP=SHR
//STMTOUT  DD DSN=CBS.PROD.STMT.PRINT(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(CYL,(20,10),RLSE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=27930)
//STMTRPT  DD DSN=CBS.PROD.RPT.STMTGEN(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP050 - CONTROL REPORTS TO DEPOSIT OPERATIONS
//*--------------------------------------------------------------------
//STEP050  EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=CBS.PROD.RPT.INTCAP(+1),DISP=SHR
//         DD DSN=CBS.PROD.RPT.FEEASSES(+1),DISP=SHR
//         DD DSN=CBS.PROD.RPT.STMTGEN(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(R,,DEPR)
//SYSIN    DD DUMMY
//
