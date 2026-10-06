      *================================================================*
      * COPYBOOK : LNPHREC                                             *
      * PURPOSE  : LOAN PAYMENT HISTORY UNLOAD RECORD. WRITTEN BY      *
      *            LNPAYAP, LOADED TO DB2 TABLE CBS.LOAN_PAYMENT_HIST  *
      *            BY THE DSNUTILB LOAD STEP IN CBSLOAN.               *
      * FILE     : LNPAYHST (SEQUENTIAL, FIXED, DISPLAY)               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2008-10-14 DPS  ORIGINAL - ONLINE PAYMENT HISTORY (CB40)       *
      *================================================================*
       01  LOAN-PAYMENT-HIST-RECORD.
           05  LPH-LOAN-NUMBER             PIC X(12).
           05  LPH-PAYMENT-DATE            PIC 9(08).
           05  LPH-TRAN-ID                 PIC X(16).
           05  LPH-SOURCE-ACCT             PIC X(12).
           05  LPH-PAYMENT-AMT             PIC 9(13)V99.
           05  LPH-APPLIED-FEES            PIC 9(07)V99.
           05  LPH-APPLIED-INT             PIC 9(11)V99.
           05  LPH-APPLIED-PRIN            PIC 9(13)V99.
           05  LPH-APPLIED-SUSPENSE        PIC 9(11)V99.
           05  LPH-PRIN-BAL-AFTER          PIC 9(13)V99.
           05  LPH-WATERFALL               PIC X(01).
           05  LPH-PAYOFF-FLAG             PIC X(01).
           05  LPH-CHANNEL                 PIC X(03).
           05  FILLER                      PIC X(10).
