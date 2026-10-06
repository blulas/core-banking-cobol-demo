      *================================================================*
      * COPYBOOK : XCPENP                                              *
      * PURPOSE  : LINKAGE FOR CHRISTMAS CLUB SAVINGS RULE MODULE XCPEN*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-24 SKN  ORIGINAL                                       *
      * 1993-05-13 PQR  PENALTY EXCEPTIONS TO OPS QUEUE CHG8255        *
      * 1994-06-14 JLM  PENALTY STATUS CODES ADDED INC4837             *
      * 1994-04-07 SKN  MISSED ROUNDING - HALF UP REQ4946              *
      * 1997-10-07 DPS  CONTRIB LIMIT REVISED INC0294                  *
      * 1998-09-02 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-06-10 EWS  GOAL ROUNDING - HALF UP INC9432                *
      * 2001-02-21 SKN  BONUS TABLE EXPANDED INC4390                   *
      * 2002-02-05 BHS  PERFORMANCE - CONTRIB READ AHEAD INC7035       *
      * 2003-03-12 BHS  PERFORMANCE - PENALTY READ AHEAD INC1852       *
      *================================================================*
       01  XCPEN-PARMS.
           05  XCPEN-MISSED-YTD            PIC S9(13)V99 COMP-3.
           05  XCPEN-GOAL-AMT              PIC S9(13)V99 COMP-3.
           05  XCPEN-WEEKLY-AMT            PIC S9(13)V99 COMP-3.
           05  XCPEN-GOAL-YTD              PIC S9(13)V99 COMP-3.
           05  XCPEN-BONUS-YTD             PIC S9(13)V99 COMP-3.
           05  XCPEN-CONTRIB-AMT           PIC S9(13)V99 COMP-3.
           05  XCPEN-LAST-PAY-DATE         PIC 9(08).
           05  XCPEN-NOTICE-DATE           PIC 9(08).
           05  XCPEN-OPEN-DATE             PIC 9(08).
           05  XCPEN-MATURITY-DATE         PIC 9(08).
           05  XCPEN-PAYOUT-METHOD-CD      PIC X(01).
               88  XCPEN-PAYOUT-METHOD-CD-B        VALUE 'B'.
               88  XCPEN-PAYOUT-METHOD-CD-H        VALUE 'H'.
               88  XCPEN-PAYOUT-METHOD-CD-U        VALUE 'U'.
               88  XCPEN-PAYOUT-METHOD-CD-W        VALUE 'W'.
           05  XCPEN-PLAN-CD               PIC X(02).
           05  XCPEN-REGION-CD             PIC X(03).
               88  XCPEN-REGION-CD-FLM             VALUE 'FLM'.
               88  XCPEN-REGION-CD-HLS             VALUE 'HLS'.
               88  XCPEN-REGION-CD-KMJ             VALUE 'KMJ'.
               88  XCPEN-REGION-CD-PDA             VALUE 'PDA'.
               88  XCPEN-REGION-CD-RVG             VALUE 'RVG'.
               88  XCPEN-REGION-CD-WMB             VALUE 'WMB'.
           05  XCPEN-TYPE-CD               PIC X(01).
               88  XCPEN-TYPE-CD-A                 VALUE 'A'.
               88  XCPEN-TYPE-CD-B                 VALUE 'B'.
               88  XCPEN-TYPE-CD-D                 VALUE 'D'.
               88  XCPEN-TYPE-CD-E                 VALUE 'E'.
               88  XCPEN-TYPE-CD-T                 VALUE 'T'.
               88  XCPEN-TYPE-CD-W                 VALUE 'W'.
           05  XCPEN-OVERRIDE-FLAG         PIC X(01).
               88  XCPEN-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  XCPEN-PREFERRED-FLAG        PIC X(01).
               88  XCPEN-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  XCPEN-STOP-FLAG             PIC X(01).
               88  XCPEN-STOP-FLAG-YES             VALUE 'Y'.
           05  XCPEN-CYCLE-CNT             PIC 9(07).
           05  XCPEN-NOTICE-CNT            PIC 9(07).
           05  XCPEN-RATE                  PIC 9(03)V9(06).
           05  XCPEN-FEE-PCT               PIC 9(03)V9(06).
           05  XCPEN-CUST-ID               PIC X(10).
           05  XCPEN-CONTACT               PIC X(20).
           05  XCPEN-RETURN-CODE           PIC 9(02).
