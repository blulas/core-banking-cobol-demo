//CBSYREND JOB (CBS,TAX),'1099-INT YEAR END',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* JOB      : CBSYREND                                               *
//* PURPOSE  : YEAR-END TAX REPORTING (INT1099) - EOD STEP 17.        *
//*            AGGREGATES INTEREST PAID YTD BY PRIMARY OWNER TAX ID,  *
//*            WRITES A 1099-INT RECORD WHERE TOTAL >= $10.00 (OR ANY *
//*            AMOUNT WITHHELD), FLAGS B-NOTICE / BACKUP WITHHOLDING, *
//*            THEN ROLLS INT-PAID-YTD TO PRIOR YEAR AND RESETS YTD.  *
//*            RUNS ONLY WHEN CTL-YEAR-END-FLAG = Y; PARM='FORCE'     *
//*            RUNS ON ANY DATE (EXTRACT AND RESET - USE WITH CARE).  *
//*            STEP030 COPIES THE T/A/B/C/F EXTRACT TO THE FIRE       *
//*            TRANSMISSION DATASET (SENT BY TAX OPERATIONS VIA NDM). *
//*            DO NOT SORT IT - IRS RECORD SEQUENCE MUST BE KEPT.     *
//*            RERUN: RESTORE ACCTMST FROM STEP010 FIRST - THE YTD    *
//*            RESET IS NOT REPEATABLE.                               *
//* CHANGE LOG                                                        *
//* 1993-01-04 RJT  ORIGINAL                                          *
//* 2001-12-17 MKL  BACKUP WITHHOLDING (BOX 4)                        *
//* 2014-12-08 DPS  FIRE TRANSMISSION COPY STEP (SORT REMOVED -       *
//*                 INC27004, B RECORDS SORTED AHEAD OF T/A)          *
//*********************************************************************
//JOBLIB   DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//*
//*--------------------------------------------------------------------
//* STEP010 - IMAGE COPY ACCTMST BEFORE YTD RESET
//*--------------------------------------------------------------------
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ACCTIN   DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//ACCTOUT  DD DSN=CBS.BKUP.ACCTMST.YREND(+1),DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(50,10),RLSE),DCB=(RECFM=VB,LRECL=468)
//SYSIN    DD *
  REPRO INFILE(ACCTIN) OUTFILE(ACCTOUT)
/*
//*
//*--------------------------------------------------------------------
//* STEP020 - INT1099  1099-INT EXTRACT AND YTD ROLL
//*--------------------------------------------------------------------
//STEP020  EXEC PGM=INT1099,COND=(0,NE,STEP010)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//CUSTMST  DD DSN=CBS.PROD.CUSTMST,DISP=SHR
//TAX1099  DD DSN=CBS.PROD.TAX.INT1099.RAW(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(30,15),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=27750)
//TAXRPT   DD DSN=CBS.PROD.RPT.INT1099(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP030 - COPY EXTRACT TO FIRE TRANSMISSION DATASET
//*--------------------------------------------------------------------
//STEP030  EXEC PGM=IEBGENER,COND=(4,LT,STEP020)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=CBS.PROD.TAX.INT1099.RAW(+1),DISP=SHR
//SYSUT2   DD DSN=CBS.PROD.TAX.INT1099(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(30,15),RLSE),
//            DCB=(RECFM=FB,LRECL=250,BLKSIZE=27750)
//SYSIN    DD DUMMY
//*
//*--------------------------------------------------------------------
//* STEP040 - CONTROL REPORT TO TAX OPERATIONS
//*--------------------------------------------------------------------
//STEP040  EXEC PGM=IEBGENER,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=CBS.PROD.RPT.INT1099(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(R,,TAXR)
//SYSIN    DD DUMMY
//
