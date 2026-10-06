      *================================================================*
      * COPYBOOK : TXPRCP                                              *
      * PURPOSE  : LINKAGE FOR TELEX WIRE MESSAGING RULE MODULE TXPRC  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-09-14 JLM  ORIGINAL                                       *
      * 2000-11-27 SKN  CORRESP CALCULATION CORRECTED PRB1459          *
      * 2002-10-21 GAF  SETTLE LIMIT REVISED INC1362                   *
      * 2004-07-20 TWH  SETTLE ROUNDING - HALF UP INC3889              *
      * 2007-07-17 BHS  QUEUED CALCULATION CORRECTED PRB8799           *
      * 2010-04-14 GAF  QUEUED FEE SCHEDULE UPDATE PRB5366             *
      *================================================================*
       01  TXPRC-PARMS.
           05  TXPRC-COVER-YTD             PIC S9(13)V99 COMP-3.
           05  TXPRC-QUEUED-AMT            PIC S9(13)V99 COMP-3.
           05  TXPRC-CORRESP-YTD           PIC S9(13)V99 COMP-3.
           05  TXPRC-SETTLE-YTD            PIC S9(13)V99 COMP-3.
           05  TXPRC-QUEUED-YTD            PIC S9(13)V99 COMP-3.
           05  TXPRC-SETTLE-AMT            PIC S9(13)V99 COMP-3.
           05  TXPRC-OPEN-DATE             PIC 9(08).
           05  TXPRC-NEXT-DUE-DATE         PIC 9(08).
           05  TXPRC-NOTICE-DATE           PIC 9(08).
           05  TXPRC-EXPIRY-DATE           PIC 9(08).
           05  TXPRC-TEST-KEY-CD           PIC X(02).
           05  TXPRC-REGION-CD             PIC X(01).
               88  TXPRC-REGION-CD-G               VALUE 'G'.
               88  TXPRC-REGION-CD-M               VALUE 'M'.
               88  TXPRC-REGION-CD-T               VALUE 'T'.
           05  TXPRC-BRANCH-CD             PIC X(02).
               88  TXPRC-BRANCH-CD-CC              VALUE 'CC'.
               88  TXPRC-BRANCH-CD-KR              VALUE 'KR'.
               88  TXPRC-BRANCH-CD-LD              VALUE 'LD'.
               88  TXPRC-BRANCH-CD-LS              VALUE 'LS'.
               88  TXPRC-BRANCH-CD-XL              VALUE 'XL'.
           05  TXPRC-REASON-CD             PIC X(03).
           05  TXPRC-NOTICE-SENT-FLAG      PIC X(01).
               88  TXPRC-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  TXPRC-STOP-FLAG             PIC X(01).
               88  TXPRC-STOP-FLAG-YES             VALUE 'Y'.
           05  TXPRC-REVIEW-FLAG           PIC X(01).
               88  TXPRC-REVIEW-FLAG-YES           VALUE 'Y'.
           05  TXPRC-PAYMENT-CNT           PIC 9(07).
           05  TXPRC-EXCEPT-CNT            PIC 9(07).
           05  TXPRC-RATE                  PIC 9(03)V9(06).
           05  TXPRC-DISCOUNT              PIC 9(03)V9(06).
           05  TXPRC-ADDR                  PIC X(30).
           05  TXPRC-NAME                  PIC X(30).
           05  TXPRC-RETURN-CODE           PIC 9(02).
