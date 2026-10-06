      *================================================================*
      * COPYBOOK : ATWAIVP                                             *
      * PURPOSE  : LINKAGE FOR ATM NETWORK SETTLEMENT RULE MODULE      *
      *            ATWAIV                                              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-06-26 SKN  ORIGINAL                                       *
      * 1995-09-11 RJT  NEW SETTLE REPORT COLUMNS CHG0174              *
      * 1998-09-01 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-11-13 RJT  ADJUST TABLE EXPANDED PRB5348                  *
      * 2002-12-09 JLM  DISPENSED ROUNDING - HALF UP CHG8798           *
      * 2003-09-11 TWH  SURCHG FEE SCHEDULE UPDATE INC8081             *
      * 2005-02-23 RJT  NEW INTERCHG REPORT COLUMNS REQ8799            *
      * 2005-07-12 EWS  SETTLE CUTOFF MOVED CHG6891                    *
      * 2006-10-23 RJT  ADJUST CUTOFF MOVED CHG6420                    *
      * 2006-02-10 TWH  NEW CASSETTE REPORT COLUMNS INC4732            *
      * 2010-09-11 TWH  CASSETTE STATUS CODES ADDED REQ5182            *
      *================================================================*
       01  ATWAIV-PARMS.
           05  ATWAIV-INTERCHG-YTD         PIC S9(13)V99 COMP-3.
           05  ATWAIV-SETTLE-AMT           PIC S9(13)V99 COMP-3.
           05  ATWAIV-ADJUST-YTD           PIC S9(13)V99 COMP-3.
           05  ATWAIV-DISPENSED-YTD        PIC S9(13)V99 COMP-3.
           05  ATWAIV-ADJUST-AMT           PIC S9(13)V99 COMP-3.
           05  ATWAIV-SETTLE-YTD           PIC S9(13)V99 COMP-3.
           05  ATWAIV-NEXT-REVIEW-DATE     PIC 9(08).
           05  ATWAIV-MATURITY-DATE        PIC 9(08).
           05  ATWAIV-LAST-PAY-DATE        PIC 9(08).
           05  ATWAIV-EFFECTIVE-DATE       PIC 9(08).
           05  ATWAIV-BRANCH-CD            PIC X(02).
               88  ATWAIV-BRANCH-CD-DU             VALUE 'DU'.
               88  ATWAIV-BRANCH-CD-HX             VALUE 'HX'.
               88  ATWAIV-BRANCH-CD-YC             VALUE 'YC'.
               88  ATWAIV-BRANCH-CD-ZB             VALUE 'ZB'.
           05  ATWAIV-TERMINAL-CD          PIC X(02).
               88  ATWAIV-TERMINAL-CD-BJ           VALUE 'BJ'.
               88  ATWAIV-TERMINAL-CD-JJ           VALUE 'JJ'.
               88  ATWAIV-TERMINAL-CD-JZ           VALUE 'JZ'.
               88  ATWAIV-TERMINAL-CD-KK           VALUE 'KK'.
           05  ATWAIV-NETWORK-CD           PIC X(01).
           05  ATWAIV-STATUS-CD            PIC X(02).
               88  ATWAIV-STATUS-CD-DF             VALUE 'DF'.
               88  ATWAIV-STATUS-CD-DP             VALUE 'DP'.
               88  ATWAIV-STATUS-CD-FJ             VALUE 'FJ'.
               88  ATWAIV-STATUS-CD-NA             VALUE 'NA'.
           05  ATWAIV-SUSPEND-FLAG         PIC X(01).
               88  ATWAIV-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  ATWAIV-NOTICE-SENT-FLAG     PIC X(01).
               88  ATWAIV-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  ATWAIV-REVIEW-FLAG          PIC X(01).
               88  ATWAIV-REVIEW-FLAG-YES          VALUE 'Y'.
           05  ATWAIV-PAYMENT-CNT          PIC 9(07).
           05  ATWAIV-ITEM-CNT             PIC 9(07).
           05  ATWAIV-FEE-PCT              PIC 9(03)V9(06).
           05  ATWAIV-RATE                 PIC 9(03)V9(06).
           05  ATWAIV-DESC                 PIC X(30).
           05  ATWAIV-NAME                 PIC X(30).
           05  ATWAIV-RETURN-CODE          PIC 9(02).
