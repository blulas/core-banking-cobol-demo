      *================================================================*
      * COPYBOOK : SBNMST                                              *
      * PURPOSE  : SAVINGS BOND SERVICES MASTER RECORD (SBNMST KSDS)   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-05-24 PQR  ORIGINAL                                       *
      * 1994-06-09 LMB  NEW FACE REPORT COLUMNS INC2795                *
      * 1998-09-04 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-15 LMB  NEW ISSUE-PRICE REPORT COLUMNS CHG9744         *
      * 2000-04-01 GAF  ISSUE-PRICE LIMIT REVISED CHG0435              *
      * 2000-10-09 DPS  TAXABLE LIMIT REVISED PRB1145                  *
      * 2000-02-12 RJT  ACCRUAL NOTICE TEXT PER LEGAL REQ7961          *
      * 2001-12-01 PQR  PERFORMANCE - PURCHASE READ AHEAD INC7994      *
      * 2005-01-25 GAF  FACE NOTICE TEXT PER LEGAL REQ0440             *
      *================================================================*
       01  SBN-MASTER-REC.
           05  SBNM-KEY                    PIC X(12).
           05  SBNM-CUST-ID                PIC X(10).
           05  SBNM-NAME                   PIC X(30).
           05  SBNM-DESC                   PIC X(30).
           05  SBNM-REF-NO                 PIC X(16).
           05  SBNM-ADDR                   PIC X(30).
           05  SBNM-CONTACT                PIC X(20).
           05  SBN-MASTER-AMOUNTS.
               10  SBNM-FACE-AMT           PIC S9(13)V99 COMP-3.
               10  SBNM-FACE-YTD           PIC S9(13)V99 COMP-3.
               10  SBNM-REDEEM-AMT         PIC S9(13)V99 COMP-3.
               10  SBNM-REDEEM-YTD         PIC S9(13)V99 COMP-3.
               10  SBNM-INTEREST-AMT       PIC S9(13)V99 COMP-3.
               10  SBNM-INTEREST-YTD       PIC S9(13)V99 COMP-3.
               10  SBNM-PURCHASE-AMT       PIC S9(13)V99 COMP-3.
               10  SBNM-PURCHASE-YTD       PIC S9(13)V99 COMP-3.
               10  SBNM-ACCRUAL-AMT        PIC S9(13)V99 COMP-3.
               10  SBNM-ACCRUAL-YTD        PIC S9(13)V99 COMP-3.
               10  SBNM-TAXABLE-AMT        PIC S9(13)V99 COMP-3.
               10  SBNM-TAXABLE-YTD        PIC S9(13)V99 COMP-3.
               10  SBNM-ISSUE-PRICE-AMT    PIC S9(13)V99 COMP-3.
               10  SBNM-ISSUE-PRICE-YTD    PIC S9(13)V99 COMP-3.
           05  SBN-MASTER-DATES.
               10  SBNM-LAST-MAINT-DATE    PIC 9(08).
               10  SBNM-EFFECTIVE-DATE     PIC 9(08).
               10  SBNM-OPEN-DATE          PIC 9(08).
               10  SBNM-EXPIRY-DATE        PIC 9(08).
               10  SBNM-MATURITY-DATE      PIC 9(08).
               10  SBNM-NEXT-DUE-DATE      PIC 9(08).
               10  SBNM-NEXT-REVIEW-DATE   PIC 9(08).
               10  SBNM-LAST-PAY-DATE      PIC 9(08).
               10  SBNM-NOTICE-DATE        PIC 9(08).
           05  SBN-MASTER-CODES.
               10  SBNM-SERIES-CD          PIC X(02).
               10  SBNM-ISSUE-TYPE-CD      PIC X(02).
                   88  SBNM-ISSUE-TYPE-CD-CW       VALUE 'CW'.
                   88  SBNM-ISSUE-TYPE-CD-DY       VALUE 'DY'.
                   88  SBNM-ISSUE-TYPE-CD-JA       VALUE 'JA'.
                   88  SBNM-ISSUE-TYPE-CD-KT       VALUE 'KT'.
                   88  SBNM-ISSUE-TYPE-CD-MF       VALUE 'MF'.
                   88  SBNM-ISSUE-TYPE-CD-SA       VALUE 'SA'.
               10  SBNM-SOURCE-CD          PIC X(01).
                   88  SBNM-SOURCE-CD-E            VALUE 'E'.
                   88  SBNM-SOURCE-CD-J            VALUE 'J'.
                   88  SBNM-SOURCE-CD-L            VALUE 'L'.
                   88  SBNM-SOURCE-CD-U            VALUE 'U'.
               10  SBNM-STATUS-CD          PIC X(02).
                   88  SBNM-STATUS-CD-CS           VALUE 'CS'.
                   88  SBNM-STATUS-CD-HF           VALUE 'HF'.
                   88  SBNM-STATUS-CD-JZ           VALUE 'JZ'.
               10  SBNM-REASON-CD          PIC X(02).
                   88  SBNM-REASON-CD-DG           VALUE 'DG'.
                   88  SBNM-REASON-CD-UK           VALUE 'UK'.
                   88  SBNM-REASON-CD-VH           VALUE 'VH'.
                   88  SBNM-REASON-CD-XD           VALUE 'XD'.
               10  SBNM-BRANCH-CD          PIC X(02).
               10  SBNM-AUTO-RENEW-FLAG    PIC X(01).
                   88  SBNM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  SBNM-NOTICE-SENT-FLAG   PIC X(01).
                   88  SBNM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  SBNM-PREFERRED-FLAG     PIC X(01).
                   88  SBNM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  SBNM-OVERRIDE-FLAG      PIC X(01).
                   88  SBNM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  SBNM-HOLD-FLAG          PIC X(01).
                   88  SBNM-HOLD-FLAG-YES          VALUE 'Y'.
               10  SBNM-REVIEW-FLAG        PIC X(01).
                   88  SBNM-REVIEW-FLAG-YES        VALUE 'Y'.
           05  SBN-MASTER-COUNTERS.
               10  SBNM-CYCLE-CNT          PIC 9(07).
               10  SBNM-EXCEPT-CNT         PIC 9(07).
               10  SBNM-RETRY-CNT          PIC 9(07).
               10  SBNM-PAYMENT-CNT        PIC 9(07).
               10  SBNM-FEE-PCT            PIC 9(03)V9(06).
               10  SBNM-MARGIN             PIC 9(03)V9(06).
               10  SBNM-PENALTY-PCT        PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
