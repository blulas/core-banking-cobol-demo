      *================================================================*
      * COPYBOOK : XCFEEP                                              *
      * PURPOSE  : LINKAGE FOR CHRISTMAS CLUB SAVINGS RULE MODULE XCFEE*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-11 GAF  ORIGINAL                                       *
      * 1992-07-10 DPS  AUDIT FINDING - CONTRIB CONTROL TOTALS PRB1913 *
      * 1993-12-04 LMB  AUDIT FINDING - PAYOUT CONTROL TOTALS REQ3610  *
      * 1993-08-23 GAF  BONUS RERUN PROTECTION PRB8773                 *
      * 1994-08-12 RJT  CONTRIB ROUNDING - HALF UP INC1044             *
      * 1994-12-09 GAF  PENALTY NOTICE TEXT PER LEGAL CHG3143          *
      * 1995-09-12 LMB  MISSED LIMIT REVISED CHG9199                   *
      * 1997-12-25 GAF  BONUS ROUNDING - HALF UP CHG7150               *
      * 1998-12-11 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-21 JLM  BONUS LIMIT REVISED CHG0481                    *
      * 2001-12-18 PQR  WEEKLY ROUNDING - HALF UP REQ8608              *
      * 2003-09-03 RJT  PENALTY RERUN PROTECTION REQ7472               *
      *================================================================*
       01  XCFEE-PARMS.
           05  XCFEE-BONUS-YTD             PIC S9(13)V99 COMP-3.
           05  XCFEE-MISSED-YTD            PIC S9(13)V99 COMP-3.
           05  XCFEE-GOAL-YTD              PIC S9(13)V99 COMP-3.
           05  XCFEE-WEEKLY-AMT            PIC S9(13)V99 COMP-3.
           05  XCFEE-GOAL-AMT              PIC S9(13)V99 COMP-3.
           05  XCFEE-CONTRIB-AMT           PIC S9(13)V99 COMP-3.
           05  XCFEE-MATURITY-DATE         PIC 9(08).
           05  XCFEE-NOTICE-DATE           PIC 9(08).
           05  XCFEE-LAST-PAY-DATE         PIC 9(08).
           05  XCFEE-NEXT-DUE-DATE         PIC 9(08).
           05  XCFEE-REGION-CD             PIC X(03).
               88  XCFEE-REGION-CD-FLM             VALUE 'FLM'.
               88  XCFEE-REGION-CD-HLS             VALUE 'HLS'.
               88  XCFEE-REGION-CD-KMJ             VALUE 'KMJ'.
               88  XCFEE-REGION-CD-PDA             VALUE 'PDA'.
               88  XCFEE-REGION-CD-RVG             VALUE 'RVG'.
               88  XCFEE-REGION-CD-WMB             VALUE 'WMB'.
           05  XCFEE-REASON-CD             PIC X(02).
           05  XCFEE-CYCLE-CD              PIC X(02).
           05  XCFEE-TYPE-CD               PIC X(01).
               88  XCFEE-TYPE-CD-A                 VALUE 'A'.
               88  XCFEE-TYPE-CD-B                 VALUE 'B'.
               88  XCFEE-TYPE-CD-D                 VALUE 'D'.
               88  XCFEE-TYPE-CD-E                 VALUE 'E'.
               88  XCFEE-TYPE-CD-T                 VALUE 'T'.
               88  XCFEE-TYPE-CD-W                 VALUE 'W'.
           05  XCFEE-AUTO-RENEW-FLAG       PIC X(01).
               88  XCFEE-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  XCFEE-EXEMPT-FLAG           PIC X(01).
               88  XCFEE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  XCFEE-STOP-FLAG             PIC X(01).
               88  XCFEE-STOP-FLAG-YES             VALUE 'Y'.
           05  XCFEE-NOTICE-CNT            PIC 9(07).
           05  XCFEE-CYCLE-CNT             PIC 9(07).
           05  XCFEE-DISCOUNT              PIC 9(03)V9(06).
           05  XCFEE-RATE                  PIC 9(03)V9(06).
           05  XCFEE-DESC                  PIC X(30).
           05  XCFEE-REF-NO                PIC X(16).
           05  XCFEE-RETURN-CODE           PIC 9(02).
