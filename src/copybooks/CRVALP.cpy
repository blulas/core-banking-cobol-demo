      *================================================================*
      * COPYBOOK : CRVALP                                              *
      * PURPOSE  : LINKAGE FOR PROPRIETARY CREDIT CARD RULE MODULE     *
      *            CRVAL                                               *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-12-09 CRP  ORIGINAL                                       *
      * 1999-06-18 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-28 PQR  PAYMENT STATUS CODES ADDED CHG3479             *
      * 2000-03-09 SKN  NEW PAYMENT REPORT COLUMNS REQ8439             *
      * 2001-01-11 JLM  PAYMENT EXCEPTIONS TO OPS QUEUE PRB7292        *
      * 2006-09-16 LMB  LATE-FEE LIMIT REVISED INC6877                 *
      * 2007-05-14 DPS  PERFORMANCE - LATE-FEE READ AHEAD CHG0571      *
      *================================================================*
       01  CRVAL-PARMS.
           05  CRVAL-FINANCE-YTD           PIC S9(13)V99 COMP-3.
           05  CRVAL-LIMIT-AMT             PIC S9(13)V99 COMP-3.
           05  CRVAL-LATE-FEE-YTD          PIC S9(13)V99 COMP-3.
           05  CRVAL-MIN-DUE-AMT           PIC S9(13)V99 COMP-3.
           05  CRVAL-MIN-DUE-YTD           PIC S9(13)V99 COMP-3.
           05  CRVAL-PAYMENT-AMT           PIC S9(13)V99 COMP-3.
           05  CRVAL-OPEN-DATE             PIC 9(08).
           05  CRVAL-MATURITY-DATE         PIC 9(08).
           05  CRVAL-EXPIRY-DATE           PIC 9(08).
           05  CRVAL-EFFECTIVE-DATE        PIC 9(08).
           05  CRVAL-TYPE-CD               PIC X(02).
               88  CRVAL-TYPE-CD-GP                VALUE 'GP'.
               88  CRVAL-TYPE-CD-UV                VALUE 'UV'.
               88  CRVAL-TYPE-CD-YK                VALUE 'YK'.
           05  CRVAL-STATUS-CD             PIC X(03).
               88  CRVAL-STATUS-CD-BAL             VALUE 'BAL'.
               88  CRVAL-STATUS-CD-DDY             VALUE 'DDY'.
               88  CRVAL-STATUS-CD-DPB             VALUE 'DPB'.
               88  CRVAL-STATUS-CD-UHF             VALUE 'UHF'.
               88  CRVAL-STATUS-CD-WLL             VALUE 'WLL'.
               88  CRVAL-STATUS-CD-XAW             VALUE 'XAW'.
           05  CRVAL-BRANCH-CD             PIC X(03).
               88  CRVAL-BRANCH-CD-CTB             VALUE 'CTB'.
               88  CRVAL-BRANCH-CD-LNX             VALUE 'LNX'.
               88  CRVAL-BRANCH-CD-MGS             VALUE 'MGS'.
               88  CRVAL-BRANCH-CD-YNJ             VALUE 'YNJ'.
           05  CRVAL-BILL-CYCLE-CD         PIC X(01).
               88  CRVAL-BILL-CYCLE-CD-B           VALUE 'B'.
               88  CRVAL-BILL-CYCLE-CD-D           VALUE 'D'.
               88  CRVAL-BILL-CYCLE-CD-E           VALUE 'E'.
               88  CRVAL-BILL-CYCLE-CD-F           VALUE 'F'.
               88  CRVAL-BILL-CYCLE-CD-H           VALUE 'H'.
               88  CRVAL-BILL-CYCLE-CD-P           VALUE 'P'.
           05  CRVAL-STOP-FLAG             PIC X(01).
               88  CRVAL-STOP-FLAG-YES             VALUE 'Y'.
           05  CRVAL-PREFERRED-FLAG        PIC X(01).
               88  CRVAL-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  CRVAL-OVERRIDE-FLAG         PIC X(01).
               88  CRVAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  CRVAL-CYCLE-CNT             PIC 9(07).
           05  CRVAL-NOTICE-CNT            PIC 9(07).
           05  CRVAL-DISCOUNT              PIC 9(03)V9(06).
           05  CRVAL-PENALTY-PCT           PIC 9(03)V9(06).
           05  CRVAL-CONTACT               PIC X(20).
           05  CRVAL-DESC                  PIC X(30).
           05  CRVAL-RETURN-CODE           PIC 9(02).
