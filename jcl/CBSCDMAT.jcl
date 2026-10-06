//CBSCDMAT JOB (CBS,DEP),'CD MATURITY',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* JOB      : CBSCDMAT                                               *
//* PURPOSE  : CD MATURITY PROCESSING (CDMATUR) - EOD STEP 14.        *
//*            STAND-ALONE RERUN VERSION OF CBSEOD STEP EOD14.        *
//*            FOR EACH CD MATURING ON OR BEFORE THE BUSINESS DATE:   *
//*            PAY ACCRUED INTEREST (DINT), THEN BY RENEWAL OPTION    *
//*              A - RENEW SAME TERM AT THE CURRENT CDT1 RATE,        *
//*                  GRACE PERIOD = OLD MATURITY + 10 DAYS            *
//*              M - MOVE PRINCIPAL TO THE PAYOUT ACCOUNT (WMAT/DMAT) *
//*              C - PAY OUT AND CLOSE                                *
//*            CDS STILL IN THEIR GRACE PERIOD ARE LISTED ONLY.       *
//*            MUST RUN AFTER INTACCR (STEP 11) SO THE MATURITY DAY   *
//*            ACCRUAL IS INCLUDED IN THE INTEREST PAID. NO PARM.     *
//* CHANGE LOG                                                        *
//* 1988-07-01 RJT  ORIGINAL                                          *
//* 2004-11-15 BHS  GRACE PERIOD - CHG0301                            *
//* 2012-08-14 DPS  CUSTMST FOR NOTICE NAME / ADDRESS                 *
//*********************************************************************
//JOBLIB   DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//*
//*--------------------------------------------------------------------
//* STEP010 - IMAGE COPY ACCTMST BEFORE UPDATE
//*--------------------------------------------------------------------
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ACCTIN   DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//ACCTOUT  DD DSN=CBS.BKUP.ACCTMST.CDMATUR(+1),DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(50,10),RLSE),DCB=(RECFM=VB,LRECL=468)
//SYSIN    DD *
  REPRO INFILE(ACCTIN) OUTFILE(ACCTOUT)
/*
//*
//*--------------------------------------------------------------------
//* STEP020 - CDMATUR  MATURITY / RENEWAL / PAYOUT
//*--------------------------------------------------------------------
//STEP020  EXEC PGM=CDMATUR,COND=(0,NE,STEP010)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//PRODMST  DD DSN=CBS.PROD.PRODMST,DISP=SHR
//CUSTMST  DD DSN=CBS.PROD.CUSTMST,DISP=SHR
//TRANPOST DD DSN=CBS.PROD.TRANPOST.DAILY,DISP=MOD
//CDMATRPT DD DSN=CBS.PROD.RPT.CDMATUR(+1),
//            DISP=(NEW,CATLG,DELETE),SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=132,BLKSIZE=27984)
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//*
//*--------------------------------------------------------------------
//* STEP030 - MATURITY NOTICES TO BRANCH / CALL CENTRE
//*--------------------------------------------------------------------
//STEP030  EXEC PGM=IEBGENER,COND=(4,LT,STEP020)
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=CBS.PROD.RPT.CDMATUR(+1),DISP=SHR
//SYSUT2   DD SYSOUT=(R,,CDNT)
//SYSIN    DD DUMMY
//
