      *================================================================*
      * COPYBOOK : XCLMST                                              *
      * PURPOSE  : CHRISTMAS CLUB SAVINGS MASTER RECORD (XCLMST KSDS)  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-21 DPS  ORIGINAL                                       *
      * 1994-08-26 JLM  AUDIT FINDING - WEEKLY CONTROL TOTALS PRB9235  *
      * 1995-12-09 LMB  PERFORMANCE - MISSED READ AHEAD PRB2965        *
      * 1995-04-17 PQR  MISSED FEE SCHEDULE UPDATE CHG1897             *
      * 1995-06-10 GAF  BONUS STATUS CODES ADDED PRB9201               *
      * 1995-12-22 PQR  NEW CONTRIB REPORT COLUMNS REQ7745             *
      * 1996-03-06 RJT  CONTRIB ROUNDING - HALF UP REQ6440             *
      * 1996-06-09 GAF  MISSED CUTOFF MOVED INC2432                    *
      * 2000-02-17 SKN  PENALTY CALCULATION CORRECTED INC3480          *
      * 2002-10-10 LMB  WEEKLY EXCEPTIONS TO OPS QUEUE REQ8780         *
      * 2003-07-09 PQR  AUDIT FINDING - ACCRUED CONTROL TOTALS PRB5441 *
      * 2004-04-05 MKL  GOAL RERUN PROTECTION PRB9624                  *
      *================================================================*
       01  XCL-MASTER-REC.
           05  XCLM-KEY                    PIC X(12).
           05  XCLM-CUST-ID                PIC X(10).
           05  XCLM-NAME                   PIC X(30).
           05  XCLM-DESC                   PIC X(30).
           05  XCLM-REF-NO                 PIC X(16).
           05  XCLM-ADDR                   PIC X(30).
           05  XCLM-CONTACT                PIC X(20).
           05  XCL-MASTER-AMOUNTS.
               10  XCLM-GOAL-AMT           PIC S9(13)V99 COMP-3.
               10  XCLM-GOAL-YTD           PIC S9(13)V99 COMP-3.
               10  XCLM-WEEKLY-AMT         PIC S9(13)V99 COMP-3.
               10  XCLM-WEEKLY-YTD         PIC S9(13)V99 COMP-3.
               10  XCLM-PAYOUT-AMT         PIC S9(13)V99 COMP-3.
               10  XCLM-PAYOUT-YTD         PIC S9(13)V99 COMP-3.
               10  XCLM-BONUS-AMT          PIC S9(13)V99 COMP-3.
               10  XCLM-BONUS-YTD          PIC S9(13)V99 COMP-3.
               10  XCLM-PENALTY-AMT        PIC S9(13)V99 COMP-3.
               10  XCLM-PENALTY-YTD        PIC S9(13)V99 COMP-3.
               10  XCLM-ACCRUED-AMT        PIC S9(13)V99 COMP-3.
               10  XCLM-ACCRUED-YTD        PIC S9(13)V99 COMP-3.
               10  XCLM-CONTRIB-AMT        PIC S9(13)V99 COMP-3.
               10  XCLM-CONTRIB-YTD        PIC S9(13)V99 COMP-3.
               10  XCLM-MISSED-AMT         PIC S9(13)V99 COMP-3.
               10  XCLM-MISSED-YTD         PIC S9(13)V99 COMP-3.
           05  XCL-MASTER-DATES.
               10  XCLM-LAST-ACT-DATE      PIC 9(08).
               10  XCLM-OPEN-DATE          PIC 9(08).
               10  XCLM-NEXT-REVIEW-DATE   PIC 9(08).
               10  XCLM-EXPIRY-DATE        PIC 9(08).
               10  XCLM-LAST-STMT-DATE     PIC 9(08).
               10  XCLM-NOTICE-DATE        PIC 9(08).
               10  XCLM-LAST-PAY-DATE      PIC 9(08).
               10  XCLM-MATURITY-DATE      PIC 9(08).
               10  XCLM-NEXT-DUE-DATE      PIC 9(08).
           05  XCL-MASTER-CODES.
               10  XCLM-PLAN-CD            PIC X(02).
               10  XCLM-PAYOUT-METHOD-CD   PIC X(01).
                   88  XCLM-PAYOUT-METHOD-CD-B     VALUE 'B'.
                   88  XCLM-PAYOUT-METHOD-CD-H     VALUE 'H'.
                   88  XCLM-PAYOUT-METHOD-CD-U     VALUE 'U'.
                   88  XCLM-PAYOUT-METHOD-CD-W     VALUE 'W'.
               10  XCLM-TYPE-CD            PIC X(01).
                   88  XCLM-TYPE-CD-A              VALUE 'A'.
                   88  XCLM-TYPE-CD-B              VALUE 'B'.
                   88  XCLM-TYPE-CD-D              VALUE 'D'.
                   88  XCLM-TYPE-CD-E              VALUE 'E'.
                   88  XCLM-TYPE-CD-T              VALUE 'T'.
                   88  XCLM-TYPE-CD-W              VALUE 'W'.
               10  XCLM-REASON-CD          PIC X(02).
               10  XCLM-CYCLE-CD           PIC X(02).
               10  XCLM-REGION-CD          PIC X(03).
                   88  XCLM-REGION-CD-FLM          VALUE 'FLM'.
                   88  XCLM-REGION-CD-HLS          VALUE 'HLS'.
                   88  XCLM-REGION-CD-KMJ          VALUE 'KMJ'.
                   88  XCLM-REGION-CD-PDA          VALUE 'PDA'.
                   88  XCLM-REGION-CD-RVG          VALUE 'RVG'.
                   88  XCLM-REGION-CD-WMB          VALUE 'WMB'.
               10  XCLM-EXEMPT-FLAG        PIC X(01).
                   88  XCLM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  XCLM-PREFERRED-FLAG     PIC X(01).
                   88  XCLM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  XCLM-AUTO-RENEW-FLAG    PIC X(01).
                   88  XCLM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  XCLM-STOP-FLAG          PIC X(01).
                   88  XCLM-STOP-FLAG-YES          VALUE 'Y'.
               10  XCLM-OVERRIDE-FLAG      PIC X(01).
                   88  XCLM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  XCLM-SUSPEND-FLAG       PIC X(01).
                   88  XCLM-SUSPEND-FLAG-YES       VALUE 'Y'.
           05  XCL-MASTER-COUNTERS.
               10  XCLM-CYCLE-CNT          PIC 9(07).
               10  XCLM-NOTICE-CNT         PIC 9(07).
               10  XCLM-RETRY-CNT          PIC 9(07).
               10  XCLM-PAYMENT-CNT        PIC 9(07).
               10  XCLM-DISCOUNT           PIC 9(03)V9(06).
               10  XCLM-FEE-PCT            PIC 9(03)V9(06).
               10  XCLM-RATE               PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
