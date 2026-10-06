      *================================================================*
      * COPYBOOK : SBNV3                                               *
      * PURPOSE  : SAVINGS BOND SERVICES EXTRACT RECORD LAYOUT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-04-12 DPS  ORIGINAL                                       *
      * 1993-04-18 RJT  INTEREST FEE SCHEDULE UPDATE INC9292           *
      * 1996-08-10 JLM  ACCRUAL RERUN PROTECTION PRB0562               *
      * 1997-09-11 EWS  NEW ISSUE-PRICE REPORT COLUMNS CHG6235         *
      * 1999-05-09 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-12-12 PQR  NEW FACE REPORT COLUMNS REQ8484                *
      * 2001-01-07 EWS  INTEREST LIMIT REVISED PRB1742                 *
      * 2002-09-13 DPS  ACCRUAL RERUN PROTECTION INC3951               *
      * 2002-05-20 SKN  FACE LIMIT REVISED CHG3918                     *
      * 2003-08-17 DPS  AUDIT FINDING - FACE CONTROL TOTALS INC8864    *
      * 2005-12-04 JLM  PURCHASE TABLE EXPANDED CHG1415                *
      *================================================================*
       01  SBN-H3-REC.
           05  SBNH3-NAME                  PIC X(30).
           05  SBNH3-DESC                  PIC X(30).
           05  SBNH3-CUST-ID               PIC X(10).
           05  SBNH3-KEY                   PIC X(12).
           05  SBN-H3-AMOUNTS.
               10  SBNH3-INTEREST-YTD      PIC S9(13)V99 COMP-3.
               10  SBNH3-TAXABLE-AMT       PIC S9(13)V99 COMP-3.
               10  SBNH3-PURCHASE-YTD      PIC S9(13)V99 COMP-3.
               10  SBNH3-ACCRUAL-YTD       PIC S9(13)V99 COMP-3.
               10  SBNH3-PURCHASE-AMT      PIC S9(13)V99 COMP-3.
               10  SBNH3-ACCRUAL-AMT       PIC S9(13)V99 COMP-3.
               10  SBNH3-ISSUE-PRICE-YTD   PIC S9(13)V99 COMP-3.
               10  SBNH3-TAXABLE-YTD       PIC S9(13)V99 COMP-3.
               10  SBNH3-ISSUE-PRICE-AMT   PIC S9(13)V99 COMP-3.
               10  SBNH3-FACE-AMT          PIC S9(13)V99 COMP-3.
               10  SBNH3-INTEREST-AMT      PIC S9(13)V99 COMP-3.
               10  SBNH3-FACE-YTD          PIC S9(13)V99 COMP-3.
               10  SBNH3-REDEEM-YTD        PIC S9(13)V99 COMP-3.
               10  SBNH3-REDEEM-AMT        PIC S9(13)V99 COMP-3.
           05  SBN-H3-DATES.
               10  SBNH3-NEXT-DUE-DATE     PIC 9(08).
               10  SBNH3-LAST-PAY-DATE     PIC 9(08).
               10  SBNH3-NOTICE-DATE       PIC 9(08).
               10  SBNH3-OPEN-DATE         PIC 9(08).
               10  SBNH3-EXPIRY-DATE       PIC 9(08).
               10  SBNH3-MATURITY-DATE     PIC 9(08).
               10  SBNH3-NEXT-REVIEW-DATE  PIC 9(08).
           05  SBN-H3-CODES.
               10  SBNH3-OVERRIDE-FLAG     PIC X(01).
                   88  SBNH3-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  SBNH3-PREFERRED-FLAG    PIC X(01).
                   88  SBNH3-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  SBNH3-NOTICE-SENT-FLAG  PIC X(01).
                   88  SBNH3-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  SBNH3-ISSUE-TYPE-CD     PIC X(02).
                   88  SBNH3-ISSUE-TYPE-CD-CW      VALUE 'CW'.
                   88  SBNH3-ISSUE-TYPE-CD-DY      VALUE 'DY'.
                   88  SBNH3-ISSUE-TYPE-CD-JA      VALUE 'JA'.
                   88  SBNH3-ISSUE-TYPE-CD-KT      VALUE 'KT'.
                   88  SBNH3-ISSUE-TYPE-CD-MF      VALUE 'MF'.
                   88  SBNH3-ISSUE-TYPE-CD-SA      VALUE 'SA'.
               10  SBNH3-REVIEW-FLAG       PIC X(01).
                   88  SBNH3-REVIEW-FLAG-YES       VALUE 'Y'.
               10  SBNH3-SOURCE-CD         PIC X(01).
                   88  SBNH3-SOURCE-CD-E           VALUE 'E'.
                   88  SBNH3-SOURCE-CD-J           VALUE 'J'.
                   88  SBNH3-SOURCE-CD-L           VALUE 'L'.
                   88  SBNH3-SOURCE-CD-U           VALUE 'U'.
               10  SBNH3-AUTO-RENEW-FLAG   PIC X(01).
                   88  SBNH3-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  SBNH3-REASON-CD         PIC X(02).
                   88  SBNH3-REASON-CD-DG          VALUE 'DG'.
                   88  SBNH3-REASON-CD-UK          VALUE 'UK'.
                   88  SBNH3-REASON-CD-VH          VALUE 'VH'.
                   88  SBNH3-REASON-CD-XD          VALUE 'XD'.
               10  SBNH3-SERIES-CD         PIC X(02).
               10  SBNH3-BRANCH-CD         PIC X(02).
           05  SBN-H3-COUNTERS.
               10  SBNH3-EXCEPT-CNT        PIC 9(07).
               10  SBNH3-MARGIN            PIC 9(03)V9(06).
               10  SBNH3-PAYMENT-CNT       PIC 9(07).
               10  SBNH3-CYCLE-CNT         PIC 9(07).
               10  SBNH3-PENALTY-PCT       PIC 9(03)V9(06).
               10  SBNH3-RETRY-CNT         PIC 9(07).
               10  SBNH3-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
