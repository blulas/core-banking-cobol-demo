      *================================================================*
      * COPYBOOK : XCLIMP                                              *
      * PURPOSE  : LINKAGE FOR CHRISTMAS CLUB SAVINGS RULE MODULE XCLIM*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 DPS  ORIGINAL                                       *
      * 1999-11-06 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-08-22 BHS  MISSED FEE SCHEDULE UPDATE PRB6439             *
      * 2001-06-18 EWS  PAYOUT FEE SCHEDULE UPDATE REQ3263             *
      * 2003-10-16 EWS  PERFORMANCE - GOAL READ AHEAD PRB9873          *
      * 2004-05-07 SKN  GOAL LIMIT REVISED INC3082                     *
      * 2004-09-26 BHS  PAYOUT CUTOFF MOVED REQ1074                    *
      * 2004-10-12 JLM  GOAL STATUS CODES ADDED CHG3534                *
      *================================================================*
       01  XCLIM-PARMS.
           05  XCLIM-PAYOUT-YTD            PIC S9(13)V99 COMP-3.
           05  XCLIM-ACCRUED-AMT           PIC S9(13)V99 COMP-3.
           05  XCLIM-GOAL-AMT              PIC S9(13)V99 COMP-3.
           05  XCLIM-PAYOUT-AMT            PIC S9(13)V99 COMP-3.
           05  XCLIM-WEEKLY-AMT            PIC S9(13)V99 COMP-3.
           05  XCLIM-PENALTY-YTD           PIC S9(13)V99 COMP-3.
           05  XCLIM-NOTICE-DATE           PIC 9(08).
           05  XCLIM-OPEN-DATE             PIC 9(08).
           05  XCLIM-LAST-PAY-DATE         PIC 9(08).
           05  XCLIM-LAST-STMT-DATE        PIC 9(08).
           05  XCLIM-REGION-CD             PIC X(03).
               88  XCLIM-REGION-CD-FLM             VALUE 'FLM'.
               88  XCLIM-REGION-CD-HLS             VALUE 'HLS'.
               88  XCLIM-REGION-CD-KMJ             VALUE 'KMJ'.
               88  XCLIM-REGION-CD-PDA             VALUE 'PDA'.
               88  XCLIM-REGION-CD-RVG             VALUE 'RVG'.
               88  XCLIM-REGION-CD-WMB             VALUE 'WMB'.
           05  XCLIM-PLAN-CD               PIC X(02).
           05  XCLIM-CYCLE-CD              PIC X(02).
           05  XCLIM-REASON-CD             PIC X(02).
           05  XCLIM-SUSPEND-FLAG          PIC X(01).
               88  XCLIM-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  XCLIM-STOP-FLAG             PIC X(01).
               88  XCLIM-STOP-FLAG-YES             VALUE 'Y'.
           05  XCLIM-PREFERRED-FLAG        PIC X(01).
               88  XCLIM-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  XCLIM-PAYMENT-CNT           PIC 9(07).
           05  XCLIM-CYCLE-CNT             PIC 9(07).
           05  XCLIM-DISCOUNT              PIC 9(03)V9(06).
           05  XCLIM-FEE-PCT               PIC 9(03)V9(06).
           05  XCLIM-REF-NO                PIC X(16).
           05  XCLIM-ADDR                  PIC X(30).
           05  XCLIM-RETURN-CODE           PIC 9(02).
