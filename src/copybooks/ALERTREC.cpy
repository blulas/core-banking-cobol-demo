      *================================================================*
      * COPYBOOK : ALERTREC                                            *
      * PURPOSE  : COMPLIANCE / OPERATIONS ALERT RECORD. WRITTEN BY    *
      *            ANY PROGRAM THAT DETECTS A CONDITION REQUIRING      *
      *            REVIEW (CTR, STRUCTURING, AML, OFAC, KYC, ETC.)     *
      * FILE     : ALERTS  (SEQUENTIAL, OPENED EXTEND BY WRITERS)      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2001-10-26 BHS  ORIGINAL - USA PATRIOT ACT                     *
      * 2013-03-04 DPS  SEVERITY, RULE ID                              *
      *================================================================*
       01  ALERT-RECORD.
           05  ALRT-DATE                   PIC 9(08).
           05  ALRT-TIME                   PIC 9(06).
           05  ALRT-TYPE                   PIC X(04).
      *        CTR1 CURRENCY TRANSACTION REPORT REQUIRED
      *        STRC POSSIBLE STRUCTURING
      *        AMLx AML MONITORING SCENARIO x
      *        OFAC SANCTIONS SCREENING HIT
      *        KYCX KYC EXPIRED / EDD REQUIRED
      *        DORM DORMANCY / ESCHEAT ACTION
      *        ODCO OVERDRAFT CHARGE-OFF
      *        LIMT LIMIT EXCEEDED
      *        LNDQ LOAN DELINQUENCY
      *        RECN RECONCILIATION BREAK
           05  ALRT-SEVERITY               PIC X(01).
               88  ALRT-LOW                          VALUE 'L'.
               88  ALRT-MEDIUM                       VALUE 'M'.
               88  ALRT-HIGH                         VALUE 'H'.
               88  ALRT-CRITICAL                     VALUE 'C'.
           05  ALRT-CUST-ID                PIC X(10).
           05  ALRT-ACCT-NUMBER            PIC X(12).
           05  ALRT-AMOUNT                 PIC S9(13)V99 COMP-3.
           05  ALRT-RULE-ID                PIC X(08).
           05  ALRT-DESCRIPTION            PIC X(80).
           05  ALRT-STATUS                 PIC X(01).
               88  ALRT-OPEN                         VALUE 'O'.
               88  ALRT-CLOSED                       VALUE 'C'.
           05  ALRT-SOURCE-PGM             PIC X(08).
           05  FILLER                      PIC X(20).
