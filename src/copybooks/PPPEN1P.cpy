      *================================================================*
      * COPYBOOK : PPPEN1P                                             *
      * PURPOSE  : LINKAGE FOR POSITIVE PAY RULE MODULE PPPEN1         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-12-13 SKN  ORIGINAL                                       *
      * 1991-06-10 BHS  PERFORMANCE - VOID READ AHEAD INC7756          *
      * 1993-08-20 GAF  PERFORMANCE - ISSUED READ AHEAD REQ8683        *
      * 1993-05-11 PQR  PERFORMANCE - ISSUED READ AHEAD PRB0223        *
      * 1995-09-07 SKN  VOID CALCULATION CORRECTED REQ7014             *
      * 1996-02-15 LMB  AUDIT FINDING - PAID CONTROL TOTALS PRB2898    *
      * 1996-09-23 PQR  ISSUED LIMIT REVISED REQ1831                   *
      * 1997-12-16 TWH  PERFORMANCE - ISSUED READ AHEAD CHG1790        *
      * 1997-06-15 SKN  PERFORMANCE - PAID READ AHEAD INC1069          *
      * 1997-02-12 GAF  STALE LIMIT REVISED INC2625                    *
      * 2001-06-23 RJT  STALE CALCULATION CORRECTED CHG1007            *
      *================================================================*
       01  PPPEN1-PARMS.
           05  PPPEN1-ISSUED-AMT           PIC S9(13)V99 COMP-3.
           05  PPPEN1-VOID-AMT             PIC S9(13)V99 COMP-3.
           05  PPPEN1-STALE-YTD            PIC S9(13)V99 COMP-3.
           05  PPPEN1-EXCEPT-AMT           PIC S9(13)V99 COMP-3.
           05  PPPEN1-PAID-AMT             PIC S9(13)V99 COMP-3.
           05  PPPEN1-PAID-YTD             PIC S9(13)V99 COMP-3.
           05  PPPEN1-CLOSE-DATE           PIC 9(08).
           05  PPPEN1-LAST-MAINT-DATE      PIC 9(08).
           05  PPPEN1-EXPIRY-DATE          PIC 9(08).
           05  PPPEN1-OPEN-DATE            PIC 9(08).
           05  PPPEN1-DECISION-CD          PIC X(03).
               88  PPPEN1-DECISION-CD-AND          VALUE 'AND'.
               88  PPPEN1-DECISION-CD-BER          VALUE 'BER'.
               88  PPPEN1-DECISION-CD-CZW          VALUE 'CZW'.
               88  PPPEN1-DECISION-CD-SBV          VALUE 'SBV'.
               88  PPPEN1-DECISION-CD-ZNG          VALUE 'ZNG'.
               88  PPPEN1-DECISION-CD-ZPS          VALUE 'ZPS'.
           05  PPPEN1-REASON-CD            PIC X(01).
               88  PPPEN1-REASON-CD-B              VALUE 'B'.
               88  PPPEN1-REASON-CD-M              VALUE 'M'.
               88  PPPEN1-REASON-CD-N              VALUE 'N'.
               88  PPPEN1-REASON-CD-U              VALUE 'U'.
               88  PPPEN1-REASON-CD-V              VALUE 'V'.
               88  PPPEN1-REASON-CD-Z              VALUE 'Z'.
           05  PPPEN1-REGION-CD            PIC X(01).
               88  PPPEN1-REGION-CD-A              VALUE 'A'.
               88  PPPEN1-REGION-CD-B              VALUE 'B'.
               88  PPPEN1-REGION-CD-D              VALUE 'D'.
               88  PPPEN1-REGION-CD-H              VALUE 'H'.
               88  PPPEN1-REGION-CD-N              VALUE 'N'.
               88  PPPEN1-REGION-CD-Y              VALUE 'Y'.
           05  PPPEN1-CYCLE-CD             PIC X(01).
               88  PPPEN1-CYCLE-CD-A               VALUE 'A'.
               88  PPPEN1-CYCLE-CD-B               VALUE 'B'.
               88  PPPEN1-CYCLE-CD-C               VALUE 'C'.
               88  PPPEN1-CYCLE-CD-R               VALUE 'R'.
               88  PPPEN1-CYCLE-CD-T               VALUE 'T'.
               88  PPPEN1-CYCLE-CD-Y               VALUE 'Y'.
           05  PPPEN1-HOLD-FLAG            PIC X(01).
               88  PPPEN1-HOLD-FLAG-YES            VALUE 'Y'.
           05  PPPEN1-OVERRIDE-FLAG        PIC X(01).
               88  PPPEN1-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  PPPEN1-NOTICE-SENT-FLAG     PIC X(01).
               88  PPPEN1-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  PPPEN1-CYCLE-CNT            PIC 9(07).
           05  PPPEN1-PAYMENT-CNT          PIC 9(07).
           05  PPPEN1-DISCOUNT             PIC 9(03)V9(06).
           05  PPPEN1-RATE                 PIC 9(03)V9(06).
           05  PPPEN1-DESC                 PIC X(30).
           05  PPPEN1-CUST-ID              PIC X(10).
           05  PPPEN1-RETURN-CODE          PIC 9(02).
