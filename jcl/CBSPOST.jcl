//CBSPOST  JOB (CBS,PROD,DEP),'CBS DEPOSIT POSTING',
//             CLASS=P,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* JOB      : CBSPOST                                                *
//* PURPOSE  : DEPOSIT POSTING STREAM. IN PRODUCTION THESE PROGRAMS   *
//*            RUN INSIDE CBSEOD AS STEPS EOD04 (TRANEDIT), EOD05     *
//*            (POSTTRAN), EOD09 (HOLDREL) AND EOD13 (ODAGING). THIS  *
//*            JOB IS THE STAND-ALONE POSTING STREAM USED BY DEPOSIT  *
//*            OPERATIONS FOR AN OFF-CYCLE RERUN AND BY THE TEST      *
//*            REGION.                                                *
//*                                                                   *
//*   STEP     PROGRAM   FUNCTION                                     *
//*   PST005   IDCAMS    BACKUP ACCTMST AND HOLDMST (EXPORT TO GDG)   *
//*   PST006   IDCAMS    DRAIN ONLINE TRANQUE (CB10/CB11) TO TRANIN   *
//*   PST010   TRANEDIT  EDIT TRANIN+ACHTRAN+PAYTRAN -> TRANEDT/REJ   *
//*   PST020   POSTTRAN  POST -> ACCTMST/HOLDMST, TRANPOST, ALERTS    *
//*   PST030   HOLDREL   RELEASE HOLDS DUE BY NEXT BUSINESS DATE      *
//*   PST040   ODAGING   OVERDRAFT AGING, SUSTAINED FEE, CHARGE-OFF   *
//*                                                                   *
//* NOTES                                                             *
//*   - TRANEDIT SORTS INTERNALLY (SORT ... INPUT/OUTPUT PROCEDURE,   *
//*     CHG0233) - NO DFSORT STEP IS NEEDED BETWEEN PST010 AND PST020.*
//*     SORTWK01-03 ARE STILL ALLOCATED FOR THE INTERNAL SORT.        *
//*   - ACHTRAN AND PAYTRAN ARE OPTIONAL. WHEN THE PAYMENT HUB SENDS  *
//*     NO FILE THE DD IS DUMMY (INC24410).                           *
//*   - TRANEDIT RC 4 = ITEMS REJECTED (SEE EDITRPT) - THE STREAM     *
//*     CONTINUES. POSTTRAN RC 4 = ITEMS FOR ACCOUNTS NOT ON FILE.    *
//*     HOLDREL RC 4 = HOLD FOR AN ACCOUNT NOT ON FILE. RC > 4 STOPS  *
//*     THE STREAM.                                                   *
//*   - POSTTRAN APPENDS NSF RETURNS TO TRANREJ (DISP=MOD) FOR ACHRET.*
//*   - ODAGING MUST RUN ONCE PER BUSINESS DAY ONLY - IT INCREMENTS   *
//*     ACCT-DAYS-OVERDRAWN UNCONDITIONALLY.                          *
//*   - RESTART: RESTORE ACCTMST/HOLDMST FROM PST005 AND RESTART AT   *
//*     PST010. POSTTRAN IS NOT RERUNNABLE ON ITS OWN.                *
//*-------------------------------------------------------------------*
//* CHANGE LOG                                                        *
//* 1988-07-01 RJT  ORIGINAL (DDA NIGHTLY)                            *
//* 1993-04-19 RJT  HOLD RELEASE STEP                                 *
//* 1996-06-03 RJT  OVERDRAFT AGING STEP                              *
//* 2002-06-17 BHS  DFSORT STEP REMOVED - TRANEDIT SORTS (CHG0233)    *
//* 2012-10-01 DPS  PAYTRAN INPUT                                     *
//* 2014-11-03 DPS  TRANQUE DRAIN STEP FOR MEMO-POSTED ONLINE ITEMS   *
//* 2019-08-12 AKW  INC24410 - ACHTRAN / PAYTRAN MAY BE DUMMY         *
//*********************************************************************
//JOBLIB   DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//         DD DSN=CEE.SCEERUN,DISP=SHR
//*
//*-------------------------------------------------------------------*
//* PST005 - BACKUP ACCOUNT AND HOLD MASTERS                          *
//*-------------------------------------------------------------------*
//PST005   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//HOLDMST  DD DSN=CBS.PROD.HOLDMST,DISP=SHR
//ACCTBK   DD DSN=CBS.PROD.ACCTMST.BKUP(+1),DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(100,20),RLSE),
//            DCB=(RECFM=FB,LRECL=464,BLKSIZE=0)
//HOLDBK   DD DSN=CBS.PROD.HOLDMST.BKUP(+1),DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=121,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(ACCTMST) OUTFILE(ACCTBK)
  REPRO INFILE(HOLDMST) OUTFILE(HOLDBK)
/*
//*-------------------------------------------------------------------*
//* PST006 - COPY THE ONLINE TRANSACTION QUEUE (ESDS) TO TRANIN       *
//*          CB10 XFERFUND / CB11 TELLTRN ITEMS ARE ALREADY MEMO-     *
//*          POSTED; POSTTRAN RELIEVES THE MEMO AMOUNTS               *
//*-------------------------------------------------------------------*
//PST006   EXEC PGM=IDCAMS,COND=(0,NE,PST005)
//SYSPRINT DD SYSOUT=*
//TRANQUE  DD DSN=CBS.PROD.TRANQUE,DISP=SHR
//TRANONL  DD DSN=CBS.PROD.TRANIN.ONLINE(+1),DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(5,5),RLSE),
//            DCB=(RECFM=FB,LRECL=281,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(TRANQUE) OUTFILE(TRANONL)
/*
//*-------------------------------------------------------------------*
//* PST010 - TRANSACTION EDIT (INTERNAL SORT TO POSTING ORDER)        *
//*-------------------------------------------------------------------*
//PST010   EXEC PGM=TRANEDIT,COND=(0,NE,PST006)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=SHR
//PRODMST  DD DSN=CBS.PROD.PRODMST,DISP=SHR
//TRANIN   DD DSN=CBS.PROD.TRANIN.CHANNEL(0),DISP=SHR
//         DD DSN=CBS.PROD.TRANIN.ONLINE(+1),DISP=SHR
//         DD DSN=CBS.PROD.TRANIN.LNAUTOPY,DISP=SHR
//ACHTRAN  DD DSN=CBS.PROD.ACHTRAN.DAILY,DISP=SHR
//PAYTRAN  DD DSN=CBS.PROD.PAYTRAN.DAILY,DISP=SHR
//TRANEDT  DD DSN=CBS.PROD.TRANEDT.DAILY,DISP=OLD
//TRANREJ  DD DSN=CBS.PROD.TRANREJ.DAILY,DISP=OLD
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(20,10))
//EDITRPT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* PST020 - POSTING ENGINE                                           *
//*          SKIPPED IF TRANEDIT RC > 4                               *
//*-------------------------------------------------------------------*
//PST020   EXEC PGM=POSTTRAN,COND=(4,LT,PST010)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//HOLDMST  DD DSN=CBS.PROD.HOLDMST,DISP=OLD
//CUSTMST  DD DSN=CBS.PROD.CUSTMST,DISP=SHR
//PRODMST  DD DSN=CBS.PROD.PRODMST,DISP=SHR
//TRANEDT  DD DSN=CBS.PROD.TRANEDT.DAILY,DISP=SHR
//TRANPOST DD DSN=CBS.PROD.TRANPOST.DAILY,DISP=MOD
//TRANREJ  DD DSN=CBS.PROD.TRANREJ.DAILY,DISP=MOD
//ALERTS   DD DSN=CBS.PROD.ALERTS,DISP=MOD
//POSTRPT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* PST030 - HOLD RELEASE (THROUGH NEXT BUSINESS DATE)                *
//*-------------------------------------------------------------------*
//PST030   EXEC PGM=HOLDREL,COND=((4,LT,PST010),(4,LT,PST020))
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//HOLDMST  DD DSN=CBS.PROD.HOLDMST,DISP=OLD
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//HOLDRPT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* PST040 - OVERDRAFT AGING / SUSTAINED OD FEE / CHARGE-OFF          *
//*-------------------------------------------------------------------*
//PST040   EXEC PGM=ODAGING,
//             COND=((4,LT,PST010),(4,LT,PST020),(4,LT,PST030))
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//ACCTMST  DD DSN=CBS.PROD.ACCTMST,DISP=OLD
//CUSTMST  DD DSN=CBS.PROD.CUSTMST,DISP=SHR
//TRANPOST DD DSN=CBS.PROD.TRANPOST.DAILY,DISP=MOD
//ALERTS   DD DSN=CBS.PROD.ALERTS,DISP=MOD
//ODAGRPT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//
