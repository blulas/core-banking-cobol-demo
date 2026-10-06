      *================================================================*
      * COPYBOOK : ATFEEP                                              *
      * PURPOSE  : LINKAGE FOR ATM NETWORK SETTLEMENT RULE MODULE ATFEE*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-03 BHS  ORIGINAL                                       *
      * 1991-11-18 PQR  CASSETTE CUTOFF MOVED REQ4131                  *
      * 1995-07-21 MKL  DISPENSED STATUS CODES ADDED PRB1589           *
      * 1997-07-25 RJT  SETTLE CUTOFF MOVED PRB9640                    *
      * 1997-11-13 TWH  SETTLE STATUS CODES ADDED PRB2773              *
      * 2002-08-23 SKN  DISPENSED NOTICE TEXT PER LEGAL REQ5834        *
      * 2003-08-12 LMB  SETTLE TABLE EXPANDED CHG7919                  *
      * 2004-04-25 BHS  CASSETTE TABLE EXPANDED PRB5888                *
      * 2008-05-25 RJT  NEW SETTLE REPORT COLUMNS REQ1246              *
      * 2008-09-14 MKL  CASSETTE CALCULATION CORRECTED PRB8653         *
      * 2010-09-03 MKL  ADJUST RERUN PROTECTION INC5679                *
      * 2010-02-21 TWH  SETTLE EXCEPTIONS TO OPS QUEUE INC2819         *
      *================================================================*
       01  ATFEE-PARMS.
           05  ATFEE-DISPENSED-YTD         PIC S9(13)V99 COMP-3.
           05  ATFEE-SETTLE-YTD            PIC S9(13)V99 COMP-3.
           05  ATFEE-ADJUST-YTD            PIC S9(13)V99 COMP-3.
           05  ATFEE-CASSETTE-AMT          PIC S9(13)V99 COMP-3.
           05  ATFEE-SETTLE-AMT            PIC S9(13)V99 COMP-3.
           05  ATFEE-INTERCHG-YTD          PIC S9(13)V99 COMP-3.
           05  ATFEE-OPEN-DATE             PIC 9(08).
           05  ATFEE-MATURITY-DATE         PIC 9(08).
           05  ATFEE-NOTICE-DATE           PIC 9(08).
           05  ATFEE-LAST-PAY-DATE         PIC 9(08).
           05  ATFEE-STATUS-CD             PIC X(02).
               88  ATFEE-STATUS-CD-DF              VALUE 'DF'.
               88  ATFEE-STATUS-CD-DP              VALUE 'DP'.
               88  ATFEE-STATUS-CD-FJ              VALUE 'FJ'.
               88  ATFEE-STATUS-CD-NA              VALUE 'NA'.
           05  ATFEE-REGION-CD             PIC X(01).
               88  ATFEE-REGION-CD-F               VALUE 'F'.
               88  ATFEE-REGION-CD-G               VALUE 'G'.
               88  ATFEE-REGION-CD-H               VALUE 'H'.
               88  ATFEE-REGION-CD-P               VALUE 'P'.
               88  ATFEE-REGION-CD-V               VALUE 'V'.
               88  ATFEE-REGION-CD-X               VALUE 'X'.
           05  ATFEE-TERMINAL-CD           PIC X(02).
               88  ATFEE-TERMINAL-CD-BJ            VALUE 'BJ'.
               88  ATFEE-TERMINAL-CD-JJ            VALUE 'JJ'.
               88  ATFEE-TERMINAL-CD-JZ            VALUE 'JZ'.
               88  ATFEE-TERMINAL-CD-KK            VALUE 'KK'.
           05  ATFEE-BRANCH-CD             PIC X(02).
               88  ATFEE-BRANCH-CD-DU              VALUE 'DU'.
               88  ATFEE-BRANCH-CD-HX              VALUE 'HX'.
               88  ATFEE-BRANCH-CD-YC              VALUE 'YC'.
               88  ATFEE-BRANCH-CD-ZB              VALUE 'ZB'.
           05  ATFEE-REVIEW-FLAG           PIC X(01).
               88  ATFEE-REVIEW-FLAG-YES           VALUE 'Y'.
           05  ATFEE-STOP-FLAG             PIC X(01).
               88  ATFEE-STOP-FLAG-YES             VALUE 'Y'.
           05  ATFEE-EXEMPT-FLAG           PIC X(01).
               88  ATFEE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  ATFEE-RETRY-CNT             PIC 9(07).
           05  ATFEE-NOTICE-CNT            PIC 9(07).
           05  ATFEE-DISCOUNT              PIC 9(03)V9(06).
           05  ATFEE-FEE-PCT               PIC 9(03)V9(06).
           05  ATFEE-ADDR                  PIC X(30).
           05  ATFEE-REF-NO                PIC X(16).
           05  ATFEE-RETURN-CODE           PIC 9(02).
