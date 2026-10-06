      *================================================================*
      * COPYBOOK : SBSTATP                                             *
      * PURPOSE  : LINKAGE FOR SAVINGS BOND SERVICES RULE MODULE SBSTAT*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-10-08 JLM  ORIGINAL                                       *
      * 1999-03-01 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-07 RJT  AUDIT FINDING - FACE CONTROL TOTALS REQ7822    *
      * 2000-06-08 MKL  ISSUE-PRICE ROUNDING - HALF UP INC5978         *
      * 2002-02-03 DPS  NEW FACE REPORT COLUMNS CHG8572                *
      * 2003-09-10 BHS  ISSUE-PRICE ROUNDING - HALF UP CHG6665         *
      * 2003-03-17 EWS  ACCRUAL CALCULATION CORRECTED CHG4215          *
      * 2004-07-10 EWS  INTEREST STATUS CODES ADDED REQ3071            *
      * 2007-10-26 BHS  INTEREST ROUNDING - HALF UP PRB5894            *
      * 2008-05-20 EWS  FACE CALCULATION CORRECTED CHG9990             *
      * 2008-07-20 JLM  NEW ACCRUAL REPORT COLUMNS REQ5681             *
      * 2009-02-17 JLM  AUDIT FINDING - REDEEM CONTROL TOTALS INC9535  *
      *================================================================*
       01  SBSTAT-PARMS.
           05  SBSTAT-ACCRUAL-AMT          PIC S9(13)V99 COMP-3.
           05  SBSTAT-TAXABLE-YTD          PIC S9(13)V99 COMP-3.
           05  SBSTAT-INTEREST-YTD         PIC S9(13)V99 COMP-3.
           05  SBSTAT-FACE-AMT             PIC S9(13)V99 COMP-3.
           05  SBSTAT-REDEEM-AMT           PIC S9(13)V99 COMP-3.
           05  SBSTAT-FACE-YTD             PIC S9(13)V99 COMP-3.
           05  SBSTAT-MATURITY-DATE        PIC 9(08).
           05  SBSTAT-EFFECTIVE-DATE       PIC 9(08).
           05  SBSTAT-NOTICE-DATE          PIC 9(08).
           05  SBSTAT-NEXT-DUE-DATE        PIC 9(08).
           05  SBSTAT-REASON-CD            PIC X(02).
               88  SBSTAT-REASON-CD-DG             VALUE 'DG'.
               88  SBSTAT-REASON-CD-UK             VALUE 'UK'.
               88  SBSTAT-REASON-CD-VH             VALUE 'VH'.
               88  SBSTAT-REASON-CD-XD             VALUE 'XD'.
           05  SBSTAT-SOURCE-CD            PIC X(01).
               88  SBSTAT-SOURCE-CD-E              VALUE 'E'.
               88  SBSTAT-SOURCE-CD-J              VALUE 'J'.
               88  SBSTAT-SOURCE-CD-L              VALUE 'L'.
               88  SBSTAT-SOURCE-CD-U              VALUE 'U'.
           05  SBSTAT-STATUS-CD            PIC X(02).
               88  SBSTAT-STATUS-CD-CS             VALUE 'CS'.
               88  SBSTAT-STATUS-CD-HF             VALUE 'HF'.
               88  SBSTAT-STATUS-CD-JZ             VALUE 'JZ'.
           05  SBSTAT-ISSUE-TYPE-CD        PIC X(02).
               88  SBSTAT-ISSUE-TYPE-CD-CW         VALUE 'CW'.
               88  SBSTAT-ISSUE-TYPE-CD-DY         VALUE 'DY'.
               88  SBSTAT-ISSUE-TYPE-CD-JA         VALUE 'JA'.
               88  SBSTAT-ISSUE-TYPE-CD-KT         VALUE 'KT'.
               88  SBSTAT-ISSUE-TYPE-CD-MF         VALUE 'MF'.
               88  SBSTAT-ISSUE-TYPE-CD-SA         VALUE 'SA'.
           05  SBSTAT-REVIEW-FLAG          PIC X(01).
               88  SBSTAT-REVIEW-FLAG-YES          VALUE 'Y'.
           05  SBSTAT-PREFERRED-FLAG       PIC X(01).
               88  SBSTAT-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  SBSTAT-AUTO-RENEW-FLAG      PIC X(01).
               88  SBSTAT-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  SBSTAT-EXCEPT-CNT           PIC 9(07).
           05  SBSTAT-RETRY-CNT            PIC 9(07).
           05  SBSTAT-MARGIN               PIC 9(03)V9(06).
           05  SBSTAT-PENALTY-PCT          PIC 9(03)V9(06).
           05  SBSTAT-DESC                 PIC X(30).
           05  SBSTAT-NAME                 PIC X(30).
           05  SBSTAT-RETURN-CODE          PIC 9(02).
