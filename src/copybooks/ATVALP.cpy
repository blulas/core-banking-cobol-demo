      *================================================================*
      * COPYBOOK : ATVALP                                              *
      * PURPOSE  : LINKAGE FOR ATM NETWORK SETTLEMENT RULE MODULE ATVAL*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-21 SKN  ORIGINAL                                       *
      * 1992-06-22 BHS  AUDIT FINDING - SETTLE CONTROL TOTALS REQ4308  *
      * 1995-07-06 MKL  CASSETTE LIMIT REVISED PRB1473                 *
      * 1995-07-04 RJT  ADJUST FEE SCHEDULE UPDATE PRB6810             *
      * 1995-11-24 DPS  SETTLE LIMIT REVISED PRB4367                   *
      * 2004-03-26 JLM  PERFORMANCE - ADJUST READ AHEAD CHG4574        *
      * 2006-07-09 GAF  NEW CASSETTE REPORT COLUMNS CHG4377            *
      * 2007-01-21 PQR  ADJUST CUTOFF MOVED INC4038                    *
      * 2007-05-27 DPS  ADJUST NOTICE TEXT PER LEGAL CHG0809           *
      * 2009-11-12 MKL  AUDIT FINDING - SURCHG CONTROL TOTALS PRB2953  *
      * 2009-07-12 MKL  PERFORMANCE - SETTLE READ AHEAD INC2104        *
      * 2010-10-27 GAF  PERFORMANCE - DISPENSED READ AHEAD CHG7957     *
      *================================================================*
       01  ATVAL-PARMS.
           05  ATVAL-ADJUST-AMT            PIC S9(13)V99 COMP-3.
           05  ATVAL-INTERCHG-YTD          PIC S9(13)V99 COMP-3.
           05  ATVAL-CASSETTE-AMT          PIC S9(13)V99 COMP-3.
           05  ATVAL-SETTLE-YTD            PIC S9(13)V99 COMP-3.
           05  ATVAL-SURCHG-AMT            PIC S9(13)V99 COMP-3.
           05  ATVAL-SURCHG-YTD            PIC S9(13)V99 COMP-3.
           05  ATVAL-NOTICE-DATE           PIC 9(08).
           05  ATVAL-NEXT-REVIEW-DATE      PIC 9(08).
           05  ATVAL-MATURITY-DATE         PIC 9(08).
           05  ATVAL-OPEN-DATE             PIC 9(08).
           05  ATVAL-TERMINAL-CD           PIC X(02).
               88  ATVAL-TERMINAL-CD-BJ            VALUE 'BJ'.
               88  ATVAL-TERMINAL-CD-JJ            VALUE 'JJ'.
               88  ATVAL-TERMINAL-CD-JZ            VALUE 'JZ'.
               88  ATVAL-TERMINAL-CD-KK            VALUE 'KK'.
           05  ATVAL-BRANCH-CD             PIC X(02).
               88  ATVAL-BRANCH-CD-DU              VALUE 'DU'.
               88  ATVAL-BRANCH-CD-HX              VALUE 'HX'.
               88  ATVAL-BRANCH-CD-YC              VALUE 'YC'.
               88  ATVAL-BRANCH-CD-ZB              VALUE 'ZB'.
           05  ATVAL-STATUS-CD             PIC X(02).
               88  ATVAL-STATUS-CD-DF              VALUE 'DF'.
               88  ATVAL-STATUS-CD-DP              VALUE 'DP'.
               88  ATVAL-STATUS-CD-FJ              VALUE 'FJ'.
               88  ATVAL-STATUS-CD-NA              VALUE 'NA'.
           05  ATVAL-REASON-CD             PIC X(02).
               88  ATVAL-REASON-CD-AK              VALUE 'AK'.
               88  ATVAL-REASON-CD-HX              VALUE 'HX'.
               88  ATVAL-REASON-CD-PR              VALUE 'PR'.
               88  ATVAL-REASON-CD-XD              VALUE 'XD'.
               88  ATVAL-REASON-CD-ZJ              VALUE 'ZJ'.
           05  ATVAL-SUSPEND-FLAG          PIC X(01).
               88  ATVAL-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  ATVAL-EXEMPT-FLAG           PIC X(01).
               88  ATVAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  ATVAL-REVIEW-FLAG           PIC X(01).
               88  ATVAL-REVIEW-FLAG-YES           VALUE 'Y'.
           05  ATVAL-NOTICE-CNT            PIC 9(07).
           05  ATVAL-PAYMENT-CNT           PIC 9(07).
           05  ATVAL-DISCOUNT              PIC 9(03)V9(06).
           05  ATVAL-RATE                  PIC 9(03)V9(06).
           05  ATVAL-CONTACT               PIC X(20).
           05  ATVAL-CUST-ID               PIC X(10).
           05  ATVAL-RETURN-CODE           PIC 9(02).
