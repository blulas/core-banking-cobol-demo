      *================================================================*
      * COPYBOOK : SBNV1                                               *
      * PURPOSE  : SAVINGS BOND SERVICES PRIOR VERSION RECORD LAYOUT   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-08-20 GAF  ORIGINAL                                       *
      * 1993-11-23 PQR  REDEEM TABLE EXPANDED INC9131                  *
      * 1996-11-08 DPS  ISSUE-PRICE ROUNDING - HALF UP PRB6810         *
      * 1997-10-13 PQR  NEW INTEREST REPORT COLUMNS PRB0530            *
      * 1998-08-05 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-06-01 BHS  INTEREST CALCULATION CORRECTED INC4448         *
      * 1999-08-23 LMB  TAXABLE EXCEPTIONS TO OPS QUEUE INC7236        *
      * 2000-08-28 PQR  ACCRUAL FEE SCHEDULE UPDATE PRB9354            *
      * 2000-03-04 EWS  ACCRUAL EXCEPTIONS TO OPS QUEUE INC3160        *
      * 2000-05-11 RJT  ISSUE-PRICE RERUN PROTECTION CHG7332           *
      * 2000-04-13 RJT  TAXABLE STATUS CODES ADDED INC4197             *
      * 2005-11-22 CRP  FACE STATUS CODES ADDED PRB2169                *
      * 2009-06-11 BHS  ACCRUAL TABLE EXPANDED PRB5103                 *
      *================================================================*
       01  SBN-H1-REC.
           05  SBNH1-KEY                   PIC X(12).
           05  SBNH1-NAME                  PIC X(30).
           05  SBNH1-REF-NO                PIC X(16).
           05  SBNH1-CUST-ID               PIC X(10).
           05  SBNH1-ADDR                  PIC X(30).
           05  SBNH1-CONTACT               PIC X(20).
           05  SBN-H1-AMOUNTS.
               10  SBNH1-REDEEM-YTD        PIC S9(13)V99 COMP-3.
               10  SBNH1-ISSUE-PRICE-AMT   PIC S9(13)V99 COMP-3.
               10  SBNH1-PURCHASE-AMT      PIC S9(13)V99 COMP-3.
               10  SBNH1-INTEREST-YTD      PIC S9(13)V99 COMP-3.
               10  SBNH1-TAXABLE-YTD       PIC S9(13)V99 COMP-3.
               10  SBNH1-INTEREST-AMT      PIC S9(13)V99 COMP-3.
               10  SBNH1-ISSUE-PRICE-YTD   PIC S9(13)V99 COMP-3.
               10  SBNH1-FACE-YTD          PIC S9(13)V99 COMP-3.
               10  SBNH1-FACE-AMT          PIC S9(13)V99 COMP-3.
               10  SBNH1-PURCHASE-YTD      PIC S9(13)V99 COMP-3.
               10  SBNH1-ACCRUAL-YTD       PIC S9(13)V99 COMP-3.
               10  SBNH1-TAXABLE-AMT       PIC S9(13)V99 COMP-3.
           05  SBN-H1-DATES.
               10  SBNH1-EXPIRY-DATE       PIC 9(08).
               10  SBNH1-NOTICE-DATE       PIC 9(08).
               10  SBNH1-OPEN-DATE         PIC 9(08).
               10  SBNH1-LAST-MAINT-DATE   PIC 9(08).
               10  SBNH1-MATURITY-DATE     PIC 9(08).
               10  SBNH1-NEXT-DUE-DATE     PIC 9(08).
               10  SBNH1-EFFECTIVE-DATE    PIC 9(08).
               10  SBNH1-LAST-PAY-DATE     PIC 9(08).
           05  SBN-H1-CODES.
               10  SBNH1-OVERRIDE-FLAG     PIC X(01).
                   88  SBNH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  SBNH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  SBNH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  SBNH1-STATUS-CD         PIC X(02).
                   88  SBNH1-STATUS-CD-CS          VALUE 'CS'.
                   88  SBNH1-STATUS-CD-HF          VALUE 'HF'.
                   88  SBNH1-STATUS-CD-JZ          VALUE 'JZ'.
               10  SBNH1-REVIEW-FLAG       PIC X(01).
                   88  SBNH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  SBNH1-HOLD-FLAG         PIC X(01).
                   88  SBNH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  SBNH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  SBNH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  SBNH1-SERIES-CD         PIC X(02).
               10  SBNH1-ISSUE-TYPE-CD     PIC X(02).
                   88  SBNH1-ISSUE-TYPE-CD-CW      VALUE 'CW'.
                   88  SBNH1-ISSUE-TYPE-CD-DY      VALUE 'DY'.
                   88  SBNH1-ISSUE-TYPE-CD-JA      VALUE 'JA'.
                   88  SBNH1-ISSUE-TYPE-CD-KT      VALUE 'KT'.
                   88  SBNH1-ISSUE-TYPE-CD-MF      VALUE 'MF'.
                   88  SBNH1-ISSUE-TYPE-CD-SA      VALUE 'SA'.
               10  SBNH1-BRANCH-CD         PIC X(02).
           05  SBN-H1-COUNTERS.
               10  SBNH1-CYCLE-CNT         PIC 9(07).
               10  SBNH1-PAYMENT-CNT       PIC 9(07).
               10  SBNH1-MARGIN            PIC 9(03)V9(06).
               10  SBNH1-RETRY-CNT         PIC 9(07).
               10  SBNH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  SBNH1-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
