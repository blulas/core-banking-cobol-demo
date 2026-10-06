      *================================================================*
      * COPYBOOK : CRCMST                                              *
      * PURPOSE  : PROPRIETARY CREDIT CARD MASTER RECORD (CRCMST KSDS) *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-08-01 GAF  ORIGINAL                                       *
      * 1996-09-23 SKN  FINANCE ROUNDING - HALF UP PRB7190             *
      * 1997-12-09 RJT  FINANCE LIMIT REVISED CHG1556                  *
      * 1997-03-07 GAF  NEW FINANCE REPORT COLUMNS PRB3200             *
      * 2000-09-21 TWH  AUDIT FINDING - PAYMENT CONTROL TOTALS CHG6280 *
      * 2001-06-13 PQR  FINANCE NOTICE TEXT PER LEGAL INC3474          *
      * 2002-03-10 SKN  CASH-ADV FEE SCHEDULE UPDATE REQ9434           *
      * 2003-09-03 MKL  CASH-ADV CALCULATION CORRECTED REQ1224         *
      * 2004-02-06 JLM  PURCHASE TABLE EXPANDED CHG1474                *
      *================================================================*
       01  CRC-MASTER-REC.
           05  CRCM-KEY                    PIC X(12).
           05  CRCM-CUST-ID                PIC X(10).
           05  CRCM-NAME                   PIC X(30).
           05  CRCM-DESC                   PIC X(30).
           05  CRCM-REF-NO                 PIC X(16).
           05  CRCM-ADDR                   PIC X(30).
           05  CRCM-CONTACT                PIC X(20).
           05  CRC-MASTER-AMOUNTS.
               10  CRCM-LIMIT-AMT          PIC S9(13)V99 COMP-3.
               10  CRCM-LIMIT-YTD          PIC S9(13)V99 COMP-3.
               10  CRCM-PURCHASE-AMT       PIC S9(13)V99 COMP-3.
               10  CRCM-PURCHASE-YTD       PIC S9(13)V99 COMP-3.
               10  CRCM-CASH-ADV-AMT       PIC S9(13)V99 COMP-3.
               10  CRCM-CASH-ADV-YTD       PIC S9(13)V99 COMP-3.
               10  CRCM-FINANCE-AMT        PIC S9(13)V99 COMP-3.
               10  CRCM-FINANCE-YTD        PIC S9(13)V99 COMP-3.
               10  CRCM-MIN-DUE-AMT        PIC S9(13)V99 COMP-3.
               10  CRCM-MIN-DUE-YTD        PIC S9(13)V99 COMP-3.
               10  CRCM-LATE-FEE-AMT       PIC S9(13)V99 COMP-3.
               10  CRCM-LATE-FEE-YTD       PIC S9(13)V99 COMP-3.
               10  CRCM-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  CRCM-PAYMENT-YTD        PIC S9(13)V99 COMP-3.
           05  CRC-MASTER-DATES.
               10  CRCM-EFFECTIVE-DATE     PIC 9(08).
               10  CRCM-OPEN-DATE          PIC 9(08).
               10  CRCM-NOTICE-DATE        PIC 9(08).
               10  CRCM-NEXT-REVIEW-DATE   PIC 9(08).
               10  CRCM-EXPIRY-DATE        PIC 9(08).
               10  CRCM-LAST-PAY-DATE      PIC 9(08).
               10  CRCM-LAST-ACT-DATE      PIC 9(08).
               10  CRCM-MATURITY-DATE      PIC 9(08).
               10  CRCM-CLOSE-DATE         PIC 9(08).
           05  CRC-MASTER-CODES.
               10  CRCM-CARD-TYPE-CD       PIC X(03).
                   88  CRCM-CARD-TYPE-CD-PGU       VALUE 'PGU'.
                   88  CRCM-CARD-TYPE-CD-TRN       VALUE 'TRN'.
                   88  CRCM-CARD-TYPE-CD-WHR       VALUE 'WHR'.
                   88  CRCM-CARD-TYPE-CD-ZWB       VALUE 'ZWB'.
               10  CRCM-BILL-CYCLE-CD      PIC X(01).
                   88  CRCM-BILL-CYCLE-CD-B        VALUE 'B'.
                   88  CRCM-BILL-CYCLE-CD-D        VALUE 'D'.
                   88  CRCM-BILL-CYCLE-CD-E        VALUE 'E'.
                   88  CRCM-BILL-CYCLE-CD-F        VALUE 'F'.
                   88  CRCM-BILL-CYCLE-CD-H        VALUE 'H'.
                   88  CRCM-BILL-CYCLE-CD-P        VALUE 'P'.
               10  CRCM-TYPE-CD            PIC X(02).
                   88  CRCM-TYPE-CD-GP             VALUE 'GP'.
                   88  CRCM-TYPE-CD-UV             VALUE 'UV'.
                   88  CRCM-TYPE-CD-YK             VALUE 'YK'.
               10  CRCM-SOURCE-CD          PIC X(01).
                   88  CRCM-SOURCE-CD-C            VALUE 'C'.
                   88  CRCM-SOURCE-CD-H            VALUE 'H'.
                   88  CRCM-SOURCE-CD-M            VALUE 'M'.
                   88  CRCM-SOURCE-CD-R            VALUE 'R'.
                   88  CRCM-SOURCE-CD-S            VALUE 'S'.
                   88  CRCM-SOURCE-CD-T            VALUE 'T'.
               10  CRCM-STATUS-CD          PIC X(03).
                   88  CRCM-STATUS-CD-BAL          VALUE 'BAL'.
                   88  CRCM-STATUS-CD-DDY          VALUE 'DDY'.
                   88  CRCM-STATUS-CD-DPB          VALUE 'DPB'.
                   88  CRCM-STATUS-CD-UHF          VALUE 'UHF'.
                   88  CRCM-STATUS-CD-WLL          VALUE 'WLL'.
                   88  CRCM-STATUS-CD-XAW          VALUE 'XAW'.
               10  CRCM-BRANCH-CD          PIC X(03).
                   88  CRCM-BRANCH-CD-CTB          VALUE 'CTB'.
                   88  CRCM-BRANCH-CD-LNX          VALUE 'LNX'.
                   88  CRCM-BRANCH-CD-MGS          VALUE 'MGS'.
                   88  CRCM-BRANCH-CD-YNJ          VALUE 'YNJ'.
               10  CRCM-EXEMPT-FLAG        PIC X(01).
                   88  CRCM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  CRCM-STOP-FLAG          PIC X(01).
                   88  CRCM-STOP-FLAG-YES          VALUE 'Y'.
               10  CRCM-NOTICE-SENT-FLAG   PIC X(01).
                   88  CRCM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  CRCM-OVERRIDE-FLAG      PIC X(01).
                   88  CRCM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  CRCM-AUTO-RENEW-FLAG    PIC X(01).
                   88  CRCM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  CRCM-PREFERRED-FLAG     PIC X(01).
                   88  CRCM-PREFERRED-FLAG-YES     VALUE 'Y'.
           05  CRC-MASTER-COUNTERS.
               10  CRCM-NOTICE-CNT         PIC 9(07).
               10  CRCM-RETRY-CNT          PIC 9(07).
               10  CRCM-CYCLE-CNT          PIC 9(07).
               10  CRCM-ITEM-CNT           PIC 9(07).
               10  CRCM-DISCOUNT           PIC 9(03)V9(06).
               10  CRCM-RATE               PIC 9(03)V9(06).
               10  CRCM-PENALTY-PCT        PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
