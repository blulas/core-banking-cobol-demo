      *================================================================*
      * COPYBOOK : XCCALP                                              *
      * PURPOSE  : LINKAGE FOR CHRISTMAS CLUB SAVINGS RULE MODULE XCCAL*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-17 LMB  ORIGINAL                                       *
      * 1994-09-14 MKL  AUDIT FINDING - ACCRUED CONTROL TOTALS PRB7277 *
      * 1994-10-01 JLM  PAYOUT NOTICE TEXT PER LEGAL REQ0590           *
      * 1996-05-19 BHS  WEEKLY ROUNDING - HALF UP PRB3342              *
      * 1997-07-04 JLM  NEW WEEKLY REPORT COLUMNS INC8567              *
      * 1998-06-16 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-07 PQR  PAYOUT RERUN PROTECTION REQ4637                *
      * 2000-03-07 BHS  WEEKLY NOTICE TEXT PER LEGAL CHG6667           *
      * 2001-06-13 MKL  PENALTY STATUS CODES ADDED CHG4621             *
      * 2003-02-21 SKN  PENALTY LIMIT REVISED PRB4844                  *
      * 2003-09-24 PQR  WEEKLY RERUN PROTECTION CHG7179                *
      * 2004-05-14 TWH  MISSED TABLE EXPANDED INC9374                  *
      * 2004-10-08 JLM  CONTRIB STATUS CODES ADDED CHG8748             *
      *================================================================*
       01  XCCAL-PARMS.
           05  XCCAL-GOAL-YTD              PIC S9(13)V99 COMP-3.
           05  XCCAL-PAYOUT-AMT            PIC S9(13)V99 COMP-3.
           05  XCCAL-WEEKLY-YTD            PIC S9(13)V99 COMP-3.
           05  XCCAL-CONTRIB-YTD           PIC S9(13)V99 COMP-3.
           05  XCCAL-MISSED-AMT            PIC S9(13)V99 COMP-3.
           05  XCCAL-PENALTY-YTD           PIC S9(13)V99 COMP-3.
           05  XCCAL-LAST-STMT-DATE        PIC 9(08).
           05  XCCAL-NOTICE-DATE           PIC 9(08).
           05  XCCAL-LAST-ACT-DATE         PIC 9(08).
           05  XCCAL-MATURITY-DATE         PIC 9(08).
           05  XCCAL-PLAN-CD               PIC X(02).
           05  XCCAL-CYCLE-CD              PIC X(02).
           05  XCCAL-REGION-CD             PIC X(03).
               88  XCCAL-REGION-CD-FLM             VALUE 'FLM'.
               88  XCCAL-REGION-CD-HLS             VALUE 'HLS'.
               88  XCCAL-REGION-CD-KMJ             VALUE 'KMJ'.
               88  XCCAL-REGION-CD-PDA             VALUE 'PDA'.
               88  XCCAL-REGION-CD-RVG             VALUE 'RVG'.
               88  XCCAL-REGION-CD-WMB             VALUE 'WMB'.
           05  XCCAL-REASON-CD             PIC X(02).
           05  XCCAL-PREFERRED-FLAG        PIC X(01).
               88  XCCAL-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  XCCAL-OVERRIDE-FLAG         PIC X(01).
               88  XCCAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  XCCAL-STOP-FLAG             PIC X(01).
               88  XCCAL-STOP-FLAG-YES             VALUE 'Y'.
           05  XCCAL-RETRY-CNT             PIC 9(07).
           05  XCCAL-CYCLE-CNT             PIC 9(07).
           05  XCCAL-FEE-PCT               PIC 9(03)V9(06).
           05  XCCAL-DISCOUNT              PIC 9(03)V9(06).
           05  XCCAL-REF-NO                PIC X(16).
           05  XCCAL-CUST-ID               PIC X(10).
           05  XCCAL-RETURN-CODE           PIC 9(02).
