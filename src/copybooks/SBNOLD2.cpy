      *================================================================*
      * COPYBOOK : SBNOLD2                                             *
      * PURPOSE  : SAVINGS BOND SERVICES PRIOR VERSION RECORD LAYOUT   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-09-16 DPS  ORIGINAL                                       *
      * 1992-06-06 CRP  ISSUE-PRICE STATUS CODES ADDED REQ7595         *
      * 2000-07-15 EWS  PERFORMANCE - FACE READ AHEAD REQ5116          *
      * 2001-09-04 RJT  NEW REDEEM REPORT COLUMNS REQ5104              *
      * 2004-06-19 TWH  INTEREST RERUN PROTECTION CHG0111              *
      * 2007-06-28 JLM  PURCHASE CALCULATION CORRECTED PRB4679         *
      *================================================================*
       01  SBN-H2-REC.
           05  SBNH2-REF-NO                PIC X(16).
           05  SBNH2-CONTACT               PIC X(20).
           05  SBNH2-CUST-ID               PIC X(10).
           05  SBNH2-KEY                   PIC X(12).
           05  SBNH2-ADDR                  PIC X(30).
           05  SBNH2-DESC                  PIC X(30).
           05  SBN-H2-AMOUNTS.
               10  SBNH2-INTEREST-AMT      PIC S9(13)V99 COMP-3.
               10  SBNH2-REDEEM-AMT        PIC S9(13)V99 COMP-3.
               10  SBNH2-TAXABLE-YTD       PIC S9(13)V99 COMP-3.
               10  SBNH2-FACE-YTD          PIC S9(13)V99 COMP-3.
               10  SBNH2-ISSUE-PRICE-YTD   PIC S9(13)V99 COMP-3.
               10  SBNH2-FACE-AMT          PIC S9(13)V99 COMP-3.
               10  SBNH2-PURCHASE-AMT      PIC S9(13)V99 COMP-3.
               10  SBNH2-TAXABLE-AMT       PIC S9(13)V99 COMP-3.
               10  SBNH2-INTEREST-YTD      PIC S9(13)V99 COMP-3.
               10  SBNH2-REDEEM-YTD        PIC S9(13)V99 COMP-3.
               10  SBNH2-ACCRUAL-AMT       PIC S9(13)V99 COMP-3.
           05  SBN-H2-DATES.
               10  SBNH2-LAST-PAY-DATE     PIC 9(08).
               10  SBNH2-NOTICE-DATE       PIC 9(08).
               10  SBNH2-MATURITY-DATE     PIC 9(08).
               10  SBNH2-EFFECTIVE-DATE    PIC 9(08).
               10  SBNH2-NEXT-DUE-DATE     PIC 9(08).
               10  SBNH2-OPEN-DATE         PIC 9(08).
               10  SBNH2-LAST-MAINT-DATE   PIC 9(08).
           05  SBN-H2-CODES.
               10  SBNH2-SERIES-CD         PIC X(02).
               10  SBNH2-STATUS-CD         PIC X(02).
                   88  SBNH2-STATUS-CD-CS          VALUE 'CS'.
                   88  SBNH2-STATUS-CD-HF          VALUE 'HF'.
                   88  SBNH2-STATUS-CD-JZ          VALUE 'JZ'.
               10  SBNH2-REVIEW-FLAG       PIC X(01).
                   88  SBNH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  SBNH2-BRANCH-CD         PIC X(02).
               10  SBNH2-ISSUE-TYPE-CD     PIC X(02).
                   88  SBNH2-ISSUE-TYPE-CD-CW      VALUE 'CW'.
                   88  SBNH2-ISSUE-TYPE-CD-DY      VALUE 'DY'.
                   88  SBNH2-ISSUE-TYPE-CD-JA      VALUE 'JA'.
                   88  SBNH2-ISSUE-TYPE-CD-KT      VALUE 'KT'.
                   88  SBNH2-ISSUE-TYPE-CD-MF      VALUE 'MF'.
                   88  SBNH2-ISSUE-TYPE-CD-SA      VALUE 'SA'.
               10  SBNH2-AUTO-RENEW-FLAG   PIC X(01).
                   88  SBNH2-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  SBNH2-HOLD-FLAG         PIC X(01).
                   88  SBNH2-HOLD-FLAG-YES         VALUE 'Y'.
               10  SBNH2-OVERRIDE-FLAG     PIC X(01).
                   88  SBNH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  SBNH2-PREFERRED-FLAG    PIC X(01).
                   88  SBNH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  SBNH2-SOURCE-CD         PIC X(01).
                   88  SBNH2-SOURCE-CD-E           VALUE 'E'.
                   88  SBNH2-SOURCE-CD-J           VALUE 'J'.
                   88  SBNH2-SOURCE-CD-L           VALUE 'L'.
                   88  SBNH2-SOURCE-CD-U           VALUE 'U'.
               10  SBNH2-REASON-CD         PIC X(02).
                   88  SBNH2-REASON-CD-DG          VALUE 'DG'.
                   88  SBNH2-REASON-CD-UK          VALUE 'UK'.
                   88  SBNH2-REASON-CD-VH          VALUE 'VH'.
                   88  SBNH2-REASON-CD-XD          VALUE 'XD'.
           05  SBN-H2-COUNTERS.
               10  SBNH2-PAYMENT-CNT       PIC 9(07).
               10  SBNH2-PENALTY-PCT       PIC 9(03)V9(06).
               10  SBNH2-EXCEPT-CNT        PIC 9(07).
               10  SBNH2-FEE-PCT           PIC 9(03)V9(06).
               10  SBNH2-RETRY-CNT         PIC 9(07).
               10  SBNH2-MARGIN            PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
