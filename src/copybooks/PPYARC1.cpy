      *================================================================*
      * COPYBOOK : PPYARC1                                             *
      * PURPOSE  : POSITIVE PAY HISTORY RECORD LAYOUT                  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-01-03 GAF  ORIGINAL                                       *
      * 1995-06-09 JLM  PERFORMANCE - ISSUED READ AHEAD CHG0828        *
      * 1996-10-08 EWS  ISSUED ROUNDING - HALF UP INC6030              *
      * 1997-01-18 JLM  STALE FEE SCHEDULE UPDATE INC3934              *
      * 2000-07-15 LMB  ISSUED NOTICE TEXT PER LEGAL PRB5146           *
      * 2000-11-15 RJT  EXCEPT FEE SCHEDULE UPDATE REQ7312             *
      * 2003-02-24 JLM  VOID RERUN PROTECTION CHG7564                  *
      *================================================================*
       01  PPY-H1-REC.
           05  PPYH1-REF-NO                PIC X(16).
           05  PPYH1-CUST-ID               PIC X(10).
           05  PPYH1-CONTACT               PIC X(20).
           05  PPYH1-DESC                  PIC X(30).
           05  PPYH1-KEY                   PIC X(12).
           05  PPY-H1-AMOUNTS.
               10  PPYH1-EXCEPT-AMT        PIC S9(13)V99 COMP-3.
               10  PPYH1-PAID-YTD          PIC S9(13)V99 COMP-3.
               10  PPYH1-ISSUED-YTD        PIC S9(13)V99 COMP-3.
               10  PPYH1-STALE-AMT         PIC S9(13)V99 COMP-3.
               10  PPYH1-PAID-AMT          PIC S9(13)V99 COMP-3.
               10  PPYH1-EXCEPT-YTD        PIC S9(13)V99 COMP-3.
               10  PPYH1-VOID-AMT          PIC S9(13)V99 COMP-3.
           05  PPY-H1-DATES.
               10  PPYH1-LAST-MAINT-DATE   PIC 9(08).
               10  PPYH1-CLOSE-DATE        PIC 9(08).
               10  PPYH1-NEXT-DUE-DATE     PIC 9(08).
               10  PPYH1-LAST-ACT-DATE     PIC 9(08).
               10  PPYH1-EXPIRY-DATE       PIC 9(08).
               10  PPYH1-OPEN-DATE         PIC 9(08).
               10  PPYH1-LAST-PAY-DATE     PIC 9(08).
               10  PPYH1-EFFECTIVE-DATE    PIC 9(08).
               10  PPYH1-NEXT-REVIEW-DATE  PIC 9(08).
           05  PPY-H1-CODES.
               10  PPYH1-OVERRIDE-FLAG     PIC X(01).
                   88  PPYH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  PPYH1-EXEMPT-FLAG       PIC X(01).
                   88  PPYH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  PPYH1-DECISION-CD       PIC X(03).
                   88  PPYH1-DECISION-CD-AND       VALUE 'AND'.
                   88  PPYH1-DECISION-CD-BER       VALUE 'BER'.
                   88  PPYH1-DECISION-CD-CZW       VALUE 'CZW'.
                   88  PPYH1-DECISION-CD-SBV       VALUE 'SBV'.
                   88  PPYH1-DECISION-CD-ZNG       VALUE 'ZNG'.
                   88  PPYH1-DECISION-CD-ZPS       VALUE 'ZPS'.
               10  PPYH1-MATCH-RULE-CD     PIC X(01).
                   88  PPYH1-MATCH-RULE-CD-A       VALUE 'A'.
                   88  PPYH1-MATCH-RULE-CD-B       VALUE 'B'.
                   88  PPYH1-MATCH-RULE-CD-F       VALUE 'F'.
                   88  PPYH1-MATCH-RULE-CD-M       VALUE 'M'.
                   88  PPYH1-MATCH-RULE-CD-S       VALUE 'S'.
                   88  PPYH1-MATCH-RULE-CD-V       VALUE 'V'.
               10  PPYH1-STOP-FLAG         PIC X(01).
                   88  PPYH1-STOP-FLAG-YES         VALUE 'Y'.
               10  PPYH1-REASON-CD         PIC X(01).
                   88  PPYH1-REASON-CD-B           VALUE 'B'.
                   88  PPYH1-REASON-CD-M           VALUE 'M'.
                   88  PPYH1-REASON-CD-N           VALUE 'N'.
                   88  PPYH1-REASON-CD-U           VALUE 'U'.
                   88  PPYH1-REASON-CD-V           VALUE 'V'.
                   88  PPYH1-REASON-CD-Z           VALUE 'Z'.
               10  PPYH1-HOLD-FLAG         PIC X(01).
                   88  PPYH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  PPYH1-REGION-CD         PIC X(01).
                   88  PPYH1-REGION-CD-A           VALUE 'A'.
                   88  PPYH1-REGION-CD-B           VALUE 'B'.
                   88  PPYH1-REGION-CD-D           VALUE 'D'.
                   88  PPYH1-REGION-CD-H           VALUE 'H'.
                   88  PPYH1-REGION-CD-N           VALUE 'N'.
                   88  PPYH1-REGION-CD-Y           VALUE 'Y'.
               10  PPYH1-CYCLE-CD          PIC X(01).
                   88  PPYH1-CYCLE-CD-A            VALUE 'A'.
                   88  PPYH1-CYCLE-CD-B            VALUE 'B'.
                   88  PPYH1-CYCLE-CD-C            VALUE 'C'.
                   88  PPYH1-CYCLE-CD-R            VALUE 'R'.
                   88  PPYH1-CYCLE-CD-T            VALUE 'T'.
                   88  PPYH1-CYCLE-CD-Y            VALUE 'Y'.
           05  PPY-H1-COUNTERS.
               10  PPYH1-RATE              PIC 9(03)V9(06).
               10  PPYH1-DISCOUNT          PIC 9(03)V9(06).
               10  PPYH1-MARGIN            PIC 9(03)V9(06).
               10  PPYH1-CYCLE-CNT         PIC 9(07).
               10  PPYH1-NOTICE-CNT        PIC 9(07).
               10  PPYH1-PAYMENT-CNT       PIC 9(07).
               10  PPYH1-EXCEPT-CNT        PIC 9(07).
           05  FILLER                      PIC X(20).
