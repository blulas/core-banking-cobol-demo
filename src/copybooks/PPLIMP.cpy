      *================================================================*
      * COPYBOOK : PPLIMP                                              *
      * PURPOSE  : LINKAGE FOR POSITIVE PAY RULE MODULE PPLIM          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-25 TWH  ORIGINAL                                       *
      * 1993-06-09 EWS  STALE CALCULATION CORRECTED INC6674            *
      * 1993-04-17 DPS  PERFORMANCE - VOID READ AHEAD INC1931          *
      * 1994-11-24 GAF  STALE NOTICE TEXT PER LEGAL REQ4784            *
      * 1995-12-19 GAF  ISSUED RERUN PROTECTION CHG2873                *
      * 1996-03-24 DPS  AUDIT FINDING - STALE CONTROL TOTALS REQ0649   *
      * 1997-02-07 DPS  PAID EXCEPTIONS TO OPS QUEUE CHG0902           *
      * 1997-02-10 MKL  VOID TABLE EXPANDED CHG2816                    *
      * 1998-04-07 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-27 DPS  PAID CUTOFF MOVED PRB1627                      *
      *================================================================*
       01  PPLIM-PARMS.
           05  PPLIM-STALE-YTD             PIC S9(13)V99 COMP-3.
           05  PPLIM-EXCEPT-YTD            PIC S9(13)V99 COMP-3.
           05  PPLIM-VOID-YTD              PIC S9(13)V99 COMP-3.
           05  PPLIM-PAID-YTD              PIC S9(13)V99 COMP-3.
           05  PPLIM-ISSUED-YTD            PIC S9(13)V99 COMP-3.
           05  PPLIM-PAID-AMT              PIC S9(13)V99 COMP-3.
           05  PPLIM-NEXT-REVIEW-DATE      PIC 9(08).
           05  PPLIM-CLOSE-DATE            PIC 9(08).
           05  PPLIM-OPEN-DATE             PIC 9(08).
           05  PPLIM-EXPIRY-DATE           PIC 9(08).
           05  PPLIM-MATCH-RULE-CD         PIC X(01).
               88  PPLIM-MATCH-RULE-CD-A           VALUE 'A'.
               88  PPLIM-MATCH-RULE-CD-B           VALUE 'B'.
               88  PPLIM-MATCH-RULE-CD-F           VALUE 'F'.
               88  PPLIM-MATCH-RULE-CD-M           VALUE 'M'.
               88  PPLIM-MATCH-RULE-CD-S           VALUE 'S'.
               88  PPLIM-MATCH-RULE-CD-V           VALUE 'V'.
           05  PPLIM-DECISION-CD           PIC X(03).
               88  PPLIM-DECISION-CD-AND           VALUE 'AND'.
               88  PPLIM-DECISION-CD-BER           VALUE 'BER'.
               88  PPLIM-DECISION-CD-CZW           VALUE 'CZW'.
               88  PPLIM-DECISION-CD-SBV           VALUE 'SBV'.
               88  PPLIM-DECISION-CD-ZNG           VALUE 'ZNG'.
               88  PPLIM-DECISION-CD-ZPS           VALUE 'ZPS'.
           05  PPLIM-REASON-CD             PIC X(01).
               88  PPLIM-REASON-CD-B               VALUE 'B'.
               88  PPLIM-REASON-CD-M               VALUE 'M'.
               88  PPLIM-REASON-CD-N               VALUE 'N'.
               88  PPLIM-REASON-CD-U               VALUE 'U'.
               88  PPLIM-REASON-CD-V               VALUE 'V'.
               88  PPLIM-REASON-CD-Z               VALUE 'Z'.
           05  PPLIM-REGION-CD             PIC X(01).
               88  PPLIM-REGION-CD-A               VALUE 'A'.
               88  PPLIM-REGION-CD-B               VALUE 'B'.
               88  PPLIM-REGION-CD-D               VALUE 'D'.
               88  PPLIM-REGION-CD-H               VALUE 'H'.
               88  PPLIM-REGION-CD-N               VALUE 'N'.
               88  PPLIM-REGION-CD-Y               VALUE 'Y'.
           05  PPLIM-NOTICE-SENT-FLAG      PIC X(01).
               88  PPLIM-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  PPLIM-AUTO-RENEW-FLAG       PIC X(01).
               88  PPLIM-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  PPLIM-EXEMPT-FLAG           PIC X(01).
               88  PPLIM-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  PPLIM-CYCLE-CNT             PIC 9(07).
           05  PPLIM-NOTICE-CNT            PIC 9(07).
           05  PPLIM-RATE                  PIC 9(03)V9(06).
           05  PPLIM-DISCOUNT              PIC 9(03)V9(06).
           05  PPLIM-NAME                  PIC X(30).
           05  PPLIM-REF-NO                PIC X(16).
           05  PPLIM-RETURN-CODE           PIC 9(02).
