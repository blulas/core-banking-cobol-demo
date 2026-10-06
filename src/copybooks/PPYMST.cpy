      *================================================================*
      * COPYBOOK : PPYMST                                              *
      * PURPOSE  : POSITIVE PAY MASTER RECORD (PPYMST KSDS)            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-16 SKN  ORIGINAL                                       *
      * 1991-06-28 RJT  PERFORMANCE - EXCEPT READ AHEAD INC5340        *
      * 1991-02-07 CRP  NEW VOID REPORT COLUMNS REQ9366                *
      * 1993-08-01 GAF  ISSUED CALCULATION CORRECTED INC9756           *
      * 1993-02-12 TWH  PAID NOTICE TEXT PER LEGAL CHG8005             *
      * 1994-03-05 SKN  STALE NOTICE TEXT PER LEGAL REQ9462            *
      * 1996-12-17 JLM  STALE FEE SCHEDULE UPDATE PRB9307              *
      * 1997-07-16 JLM  VOID EXCEPTIONS TO OPS QUEUE INC4560           *
      * 1997-07-12 CRP  NEW VOID REPORT COLUMNS PRB5531                *
      * 1998-12-23 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-10-09 SKN  VOID ROUNDING - HALF UP CHG9532                *
      * 2003-01-22 GAF  AUDIT FINDING - EXCEPT CONTROL TOTALS REQ8370  *
      *================================================================*
       01  PPY-MASTER-REC.
           05  PPYM-KEY                    PIC X(12).
           05  PPYM-CUST-ID                PIC X(10).
           05  PPYM-NAME                   PIC X(30).
           05  PPYM-DESC                   PIC X(30).
           05  PPYM-REF-NO                 PIC X(16).
           05  PPYM-ADDR                   PIC X(30).
           05  PPYM-CONTACT                PIC X(20).
           05  PPY-MASTER-AMOUNTS.
               10  PPYM-ISSUED-AMT         PIC S9(13)V99 COMP-3.
               10  PPYM-ISSUED-YTD         PIC S9(13)V99 COMP-3.
               10  PPYM-PAID-AMT           PIC S9(13)V99 COMP-3.
               10  PPYM-PAID-YTD           PIC S9(13)V99 COMP-3.
               10  PPYM-EXCEPT-AMT         PIC S9(13)V99 COMP-3.
               10  PPYM-EXCEPT-YTD         PIC S9(13)V99 COMP-3.
               10  PPYM-STALE-AMT          PIC S9(13)V99 COMP-3.
               10  PPYM-STALE-YTD          PIC S9(13)V99 COMP-3.
               10  PPYM-VOID-AMT           PIC S9(13)V99 COMP-3.
               10  PPYM-VOID-YTD           PIC S9(13)V99 COMP-3.
           05  PPY-MASTER-DATES.
               10  PPYM-NEXT-DUE-DATE      PIC 9(08).
               10  PPYM-CLOSE-DATE         PIC 9(08).
               10  PPYM-EFFECTIVE-DATE     PIC 9(08).
               10  PPYM-LAST-PAY-DATE      PIC 9(08).
               10  PPYM-LAST-ACT-DATE      PIC 9(08).
               10  PPYM-OPEN-DATE          PIC 9(08).
               10  PPYM-LAST-MAINT-DATE    PIC 9(08).
               10  PPYM-EXPIRY-DATE        PIC 9(08).
               10  PPYM-NEXT-REVIEW-DATE   PIC 9(08).
           05  PPY-MASTER-CODES.
               10  PPYM-MATCH-RULE-CD      PIC X(01).
                   88  PPYM-MATCH-RULE-CD-A        VALUE 'A'.
                   88  PPYM-MATCH-RULE-CD-B        VALUE 'B'.
                   88  PPYM-MATCH-RULE-CD-F        VALUE 'F'.
                   88  PPYM-MATCH-RULE-CD-M        VALUE 'M'.
                   88  PPYM-MATCH-RULE-CD-S        VALUE 'S'.
                   88  PPYM-MATCH-RULE-CD-V        VALUE 'V'.
               10  PPYM-DECISION-CD        PIC X(03).
                   88  PPYM-DECISION-CD-AND        VALUE 'AND'.
                   88  PPYM-DECISION-CD-BER        VALUE 'BER'.
                   88  PPYM-DECISION-CD-CZW        VALUE 'CZW'.
                   88  PPYM-DECISION-CD-SBV        VALUE 'SBV'.
                   88  PPYM-DECISION-CD-ZNG        VALUE 'ZNG'.
                   88  PPYM-DECISION-CD-ZPS        VALUE 'ZPS'.
               10  PPYM-REGION-CD          PIC X(01).
                   88  PPYM-REGION-CD-A            VALUE 'A'.
                   88  PPYM-REGION-CD-B            VALUE 'B'.
                   88  PPYM-REGION-CD-D            VALUE 'D'.
                   88  PPYM-REGION-CD-H            VALUE 'H'.
                   88  PPYM-REGION-CD-N            VALUE 'N'.
                   88  PPYM-REGION-CD-Y            VALUE 'Y'.
               10  PPYM-STATUS-CD          PIC X(02).
                   88  PPYM-STATUS-CD-DR           VALUE 'DR'.
                   88  PPYM-STATUS-CD-TK           VALUE 'TK'.
                   88  PPYM-STATUS-CD-WX           VALUE 'WX'.
               10  PPYM-CYCLE-CD           PIC X(01).
                   88  PPYM-CYCLE-CD-A             VALUE 'A'.
                   88  PPYM-CYCLE-CD-B             VALUE 'B'.
                   88  PPYM-CYCLE-CD-C             VALUE 'C'.
                   88  PPYM-CYCLE-CD-R             VALUE 'R'.
                   88  PPYM-CYCLE-CD-T             VALUE 'T'.
                   88  PPYM-CYCLE-CD-Y             VALUE 'Y'.
               10  PPYM-REASON-CD          PIC X(01).
                   88  PPYM-REASON-CD-B            VALUE 'B'.
                   88  PPYM-REASON-CD-M            VALUE 'M'.
                   88  PPYM-REASON-CD-N            VALUE 'N'.
                   88  PPYM-REASON-CD-U            VALUE 'U'.
                   88  PPYM-REASON-CD-V            VALUE 'V'.
                   88  PPYM-REASON-CD-Z            VALUE 'Z'.
               10  PPYM-OVERRIDE-FLAG      PIC X(01).
                   88  PPYM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  PPYM-EXEMPT-FLAG        PIC X(01).
                   88  PPYM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  PPYM-AUTO-RENEW-FLAG    PIC X(01).
                   88  PPYM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  PPYM-NOTICE-SENT-FLAG   PIC X(01).
                   88  PPYM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  PPYM-STOP-FLAG          PIC X(01).
                   88  PPYM-STOP-FLAG-YES          VALUE 'Y'.
               10  PPYM-HOLD-FLAG          PIC X(01).
                   88  PPYM-HOLD-FLAG-YES          VALUE 'Y'.
           05  PPY-MASTER-COUNTERS.
               10  PPYM-EXCEPT-CNT         PIC 9(07).
               10  PPYM-CYCLE-CNT          PIC 9(07).
               10  PPYM-PAYMENT-CNT        PIC 9(07).
               10  PPYM-NOTICE-CNT         PIC 9(07).
               10  PPYM-RATE               PIC 9(03)V9(06).
               10  PPYM-MARGIN             PIC 9(03)V9(06).
               10  PPYM-DISCOUNT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
