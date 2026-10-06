      *================================================================*
      * COPYBOOK : ATTIERP                                             *
      * PURPOSE  : LINKAGE FOR ATM NETWORK SETTLEMENT RULE MODULE      *
      *            ATTIER                                              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-25 LMB  ORIGINAL                                       *
      * 1993-06-09 MKL  SURCHG TABLE EXPANDED INC5890                  *
      * 1995-09-24 LMB  PERFORMANCE - ADJUST READ AHEAD INC8666        *
      * 1995-07-14 LMB  SETTLE RERUN PROTECTION REQ2053                *
      * 1999-08-22 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-07-23 BHS  DISPENSED RERUN PROTECTION REQ5190             *
      * 2005-04-27 BHS  NEW SETTLE REPORT COLUMNS CHG9320              *
      * 2008-10-07 BHS  ADJUST RERUN PROTECTION REQ8716                *
      * 2008-02-06 TWH  CASSETTE CALCULATION CORRECTED INC9496         *
      * 2009-09-01 SKN  DISPENSED NOTICE TEXT PER LEGAL PRB6655        *
      *================================================================*
       01  ATTIER-PARMS.
           05  ATTIER-SETTLE-AMT           PIC S9(13)V99 COMP-3.
           05  ATTIER-INTERCHG-YTD         PIC S9(13)V99 COMP-3.
           05  ATTIER-ADJUST-YTD           PIC S9(13)V99 COMP-3.
           05  ATTIER-ADJUST-AMT           PIC S9(13)V99 COMP-3.
           05  ATTIER-CASSETTE-YTD         PIC S9(13)V99 COMP-3.
           05  ATTIER-SURCHG-AMT           PIC S9(13)V99 COMP-3.
           05  ATTIER-NOTICE-DATE          PIC 9(08).
           05  ATTIER-EXPIRY-DATE          PIC 9(08).
           05  ATTIER-NEXT-REVIEW-DATE     PIC 9(08).
           05  ATTIER-NEXT-DUE-DATE        PIC 9(08).
           05  ATTIER-BRANCH-CD            PIC X(02).
               88  ATTIER-BRANCH-CD-DU             VALUE 'DU'.
               88  ATTIER-BRANCH-CD-HX             VALUE 'HX'.
               88  ATTIER-BRANCH-CD-YC             VALUE 'YC'.
               88  ATTIER-BRANCH-CD-ZB             VALUE 'ZB'.
           05  ATTIER-STATUS-CD            PIC X(02).
               88  ATTIER-STATUS-CD-DF             VALUE 'DF'.
               88  ATTIER-STATUS-CD-DP             VALUE 'DP'.
               88  ATTIER-STATUS-CD-FJ             VALUE 'FJ'.
               88  ATTIER-STATUS-CD-NA             VALUE 'NA'.
           05  ATTIER-REGION-CD            PIC X(01).
               88  ATTIER-REGION-CD-F              VALUE 'F'.
               88  ATTIER-REGION-CD-G              VALUE 'G'.
               88  ATTIER-REGION-CD-H              VALUE 'H'.
               88  ATTIER-REGION-CD-P              VALUE 'P'.
               88  ATTIER-REGION-CD-V              VALUE 'V'.
               88  ATTIER-REGION-CD-X              VALUE 'X'.
           05  ATTIER-REASON-CD            PIC X(02).
               88  ATTIER-REASON-CD-AK             VALUE 'AK'.
               88  ATTIER-REASON-CD-HX             VALUE 'HX'.
               88  ATTIER-REASON-CD-PR             VALUE 'PR'.
               88  ATTIER-REASON-CD-XD             VALUE 'XD'.
               88  ATTIER-REASON-CD-ZJ             VALUE 'ZJ'.
           05  ATTIER-STOP-FLAG            PIC X(01).
               88  ATTIER-STOP-FLAG-YES            VALUE 'Y'.
           05  ATTIER-EXEMPT-FLAG          PIC X(01).
               88  ATTIER-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  ATTIER-NOTICE-SENT-FLAG     PIC X(01).
               88  ATTIER-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  ATTIER-ITEM-CNT             PIC 9(07).
           05  ATTIER-RETRY-CNT            PIC 9(07).
           05  ATTIER-FEE-PCT              PIC 9(03)V9(06).
           05  ATTIER-RATE                 PIC 9(03)V9(06).
           05  ATTIER-DESC                 PIC X(30).
           05  ATTIER-CUST-ID              PIC X(10).
           05  ATTIER-RETURN-CODE          PIC 9(02).
