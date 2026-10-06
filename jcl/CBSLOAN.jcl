//CBSLOAN  JOB (CBS,PROD,LND),'CBS LOAN SERVICING',
//             CLASS=P,MSGCLASS=X,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0M
//*********************************************************************
//* JOB      : CBSLOAN                                                *
//* PURPOSE  : LOAN SERVICING STREAM. IN PRODUCTION THESE PROGRAMS    *
//*            RUN INSIDE CBSEOD AS STEPS EOD07 (LNPAYAP), EOD12      *
//*            (LNACCR) AND EOD15A/B (LNBILL, LNDELQ). THIS JOB IS    *
//*            THE STAND-ALONE LOAN STREAM USED BY LOAN OPERATIONS    *
//*            FOR AN OFF-CYCLE RERUN AND BY THE TEST REGION.         *
//*                                                                   *
//*   STEP     PROGRAM   FUNCTION                                     *
//*   LNS005   IDCAMS    BACKUP LOANMST (EXPORT TO GDG)               *
//*   LNS010   LNPAYAP   APPLY POSTED WLNP PAYMENTS (TRANPOST)        *
//*   LNS015   DSNUTILB  LOAD LNPAYHST -> CBS.LOAN_PAYMENT_HIST (CB40)*
//*   LNS020   LNACCR    DAILY INTEREST ACCRUAL / VARIABLE RATE RESET *
//*   LNS030   LNBILL    BILLS (LNBILLS) + AUTOPAY DEBITS (LNAUTOPY)  *
//*   LNS040   LNDELQ    AGING, LATE FEES, NON-ACCRUAL, CHARGE-OFF    *
//*   LNS050   SORT      LNBILLS BY DUE DATE FOR THE STATEMENT VENDOR *
//*                                                                   *
//* NOTES                                                             *
//*   - ORDER MATTERS: LNPAYAP BEFORE LNACCR (A PAYMENT ON THE DUE    *
//*     DATE CLEARS BILLED INTEREST BEFORE TODAY'S ACCRUAL), LNACCR   *
//*     BEFORE LNBILL (BILLED INTEREST USES TODAY'S ACCRUED), LNBILL  *
//*     BEFORE LNDELQ (LNDELQ ROLLS THE NEXT DUE DATE).               *
//*   - LNAUTOPY IS WRITTEN DISP=MOD TO CBS.PROD.TRANIN.LNAUTOPY,     *
//*     WHICH THE NEXT CYCLE'S TRANIN CONCATENATION PICKS UP. THE     *
//*     DEBITS ARE EDITED/POSTED TO THE DEPOSIT ACCOUNT BY TRANEDIT / *
//*     POSTTRAN AND APPLIED TO THE LOAN BY LNPAYAP ON THAT CYCLE.    *
//*     DO NOT RERUN LNS030 FOR THE SAME BUSINESS DATE WITHOUT        *
//*     CLEARING THE DATASET - LOAN-AUTOPAY-GEN-DUE-DATE PREVENTS A   *
//*     SECOND DEBIT FOR THE SAME DUE DATE ONLY ON LOANMST.           *
//*   - LNPAYAP RC 4 = PAYMENT(S) FOR UNKNOWN LOANS ON THE EXCEPTION  *
//*     REPORT; THE JOB CONTINUES. RC > 4 STOPS THE STREAM.           *
//*   - RESTART: RESTORE LOANMST FROM LNS005 BACKUP, THEN RESTART AT  *
//*     LNS010. STEPS ARE NOT INDIVIDUALLY RERUNNABLE (LNACCR IS -    *
//*     IT SKIPS LOANS ALREADY ACCRUED THROUGH THE BUSINESS DATE).    *
//*-------------------------------------------------------------------*
//* CHANGE LOG                                                        *
//* 1991-05-06 RJT  ORIGINAL (CLS NIGHTLY)                            *
//* 2006-02-13 BHS  DUE TABLE CONVERSION - CHG0412                    *
//* 2008-10-14 DPS  PAYMENT HISTORY LOAD FOR CB40                     *
//* 2016-04-25 AKW  LPER - LNPAYAP READS TRANPOST                     *
//* 2017-03-20 AKW  LNAUTOPY TO NEXT CYCLE TRANIN - CHG2104           *
//*********************************************************************
//JOBLIB   DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//         DD DSN=DSN.V13R1.SDSNLOAD,DISP=SHR
//*
//*-------------------------------------------------------------------*
//* LNS005 - BACKUP LOAN MASTER                                       *
//*-------------------------------------------------------------------*
//LNS005   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//LOANMST  DD DSN=CBS.PROD.LOANMST,DISP=SHR
//BACKUP   DD DSN=CBS.PROD.LOANMST.BKUP(+1),DISP=(NEW,CATLG),
//            UNIT=SYSDA,SPACE=(CYL,(50,10),RLSE),
//            DCB=(RECFM=VB,LRECL=1004,BLKSIZE=0)
//SYSIN    DD *
  REPRO INFILE(LOANMST) OUTFILE(BACKUP)
/*
//*-------------------------------------------------------------------*
//* LNS010 - LOAN PAYMENT APPLICATION                                 *
//*-------------------------------------------------------------------*
//LNS010   EXEC PGM=LNPAYAP,COND=(0,NE,LNS005)
//STEPLIB  DD DSN=CBS.PROD.LOADLIB,DISP=SHR
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//TRANPOST DD DSN=CBS.PROD.TRANPOST(0),DISP=SHR
//LOANMST  DD DSN=CBS.PROD.LOANMST,DISP=SHR
//LNPAYHST DD DSN=CBS.PROD.LNPAYHST(+1),DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=143,BLKSIZE=0)
//LNPYRPT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* LNS015 - LOAD PAYMENT HISTORY TO DB2 (RESUME YES)                 *
//*-------------------------------------------------------------------*
//LNS015   EXEC PGM=DSNUTILB,PARM='DB2P,CBSLNPH',COND=(4,LT,LNS010)
//SYSPRINT DD SYSOUT=*
//UTPRINT  DD SYSOUT=*
//SYSREC   DD DSN=CBS.PROD.LNPAYHST(+1),DISP=SHR
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(5,5))
//SORTOUT  DD UNIT=SYSDA,SPACE=(CYL,(5,5))
//SYSIN    DD *
  LOAD DATA RESUME YES LOG NO
    INTO TABLE CBS.LOAN_PAYMENT_HIST
    ( LOAN_NUMBER      POSITION(1:12)    CHAR(12),
      PAYMENT_DATE     POSITION(13:20)   DATE EXTERNAL(8),
      TRAN_ID          POSITION(21:36)   CHAR(16),
      SOURCE_ACCT      POSITION(37:48)   CHAR(12),
      PAYMENT_AMT      POSITION(49:63)   DECIMAL ZONED(15,2),
      APPLIED_FEES     POSITION(64:72)   DECIMAL ZONED(9,2),
      APPLIED_INT      POSITION(73:85)   DECIMAL ZONED(13,2),
      APPLIED_PRIN     POSITION(86:100)  DECIMAL ZONED(15,2),
      APPLIED_SUSPENSE POSITION(101:113) DECIMAL ZONED(13,2),
      PRIN_BAL_AFTER   POSITION(114:128) DECIMAL ZONED(15,2),
      WATERFALL        POSITION(129:129) CHAR(1),
      PAYOFF_FLAG      POSITION(130:130) CHAR(1),
      CHANNEL          POSITION(131:133) CHAR(3) )
/*
//*-------------------------------------------------------------------*
//* LNS020 - DAILY LOAN INTEREST ACCRUAL                              *
//*-------------------------------------------------------------------*
//LNS020   EXEC PGM=LNACCR,COND=(4,LT,LNS010)
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//LOANMST  DD DSN=CBS.PROD.LOANMST,DISP=SHR
//LNACRPT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* LNS030 - BILLING AND AUTOPAY                                      *
//*-------------------------------------------------------------------*
//LNS030   EXEC PGM=LNBILL,COND=((4,LT,LNS010),(0,NE,LNS020))
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//LOANMST  DD DSN=CBS.PROD.LOANMST,DISP=SHR
//LNBILLS  DD DSN=CBS.PROD.LNBILLS(+1),DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=176,BLKSIZE=0)
//LNAUTOPY DD DSN=CBS.PROD.TRANIN.LNAUTOPY,DISP=MOD
//LNBLRPT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* LNS040 - DELINQUENCY, LATE FEES, NON-ACCRUAL, CHARGE-OFF          *
//*-------------------------------------------------------------------*
//LNS040   EXEC PGM=LNDELQ,COND=((4,LT,LNS010),(0,NE,LNS030))
//BANKCTL  DD DSN=CBS.PROD.BANKCTL,DISP=SHR
//LOANMST  DD DSN=CBS.PROD.LOANMST,DISP=SHR
//ALERTS   DD DSN=CBS.PROD.ALERTS,DISP=MOD
//LNDQRPT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//*-------------------------------------------------------------------*
//* LNS050 - SORT BILLS BY DUE DATE / LOAN FOR STATEMENT VENDOR       *
//*-------------------------------------------------------------------*
//LNS050   EXEC PGM=SORT,COND=(0,NE,LNS030)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=CBS.PROD.LNBILLS(+1),DISP=SHR
//SORTOUT  DD DSN=CBS.PROD.STMTVEND.LNBILLS(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(TRK,(15,15),RLSE),
//            DCB=(RECFM=FB,LRECL=176,BLKSIZE=0)
//SYSIN    DD *
  SORT FIELDS=(35,8,CH,A,1,12,CH,A)
/*
