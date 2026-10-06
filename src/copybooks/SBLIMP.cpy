      *================================================================*
      * COPYBOOK : SBLIMP                                              *
      * PURPOSE  : LINKAGE FOR SAVINGS BOND SERVICES RULE MODULE SBLIM *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-05-21 CRP  ORIGINAL                                       *
      * 1994-11-04 EWS  REDEEM CALCULATION CORRECTED REQ8333           *
      * 1995-06-22 BHS  INTEREST ROUNDING - HALF UP REQ5960            *
      * 1996-11-12 TWH  FACE EXCEPTIONS TO OPS QUEUE REQ1372           *
      * 1998-10-10 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-03-11 PQR  REDEEM CUTOFF MOVED CHG3497                    *
      * 2000-04-23 LMB  PURCHASE ROUNDING - HALF UP REQ6162            *
      * 2000-12-01 MKL  PURCHASE LIMIT REVISED PRB8333                 *
      * 2005-04-06 LMB  ACCRUAL CALCULATION CORRECTED PRB8200          *
      * 2006-01-04 JLM  ISSUE-PRICE TABLE EXPANDED CHG2703             *
      * 2009-06-16 PQR  ISSUE-PRICE CUTOFF MOVED INC5064               *
      *================================================================*
       01  SBLIM-PARMS.
           05  SBLIM-ISSUE-PRICE-YTD       PIC S9(13)V99 COMP-3.
           05  SBLIM-FACE-YTD              PIC S9(13)V99 COMP-3.
           05  SBLIM-PURCHASE-YTD          PIC S9(13)V99 COMP-3.
           05  SBLIM-TAXABLE-AMT           PIC S9(13)V99 COMP-3.
           05  SBLIM-PURCHASE-AMT          PIC S9(13)V99 COMP-3.
           05  SBLIM-TAXABLE-YTD           PIC S9(13)V99 COMP-3.
           05  SBLIM-LAST-MAINT-DATE       PIC 9(08).
           05  SBLIM-NOTICE-DATE           PIC 9(08).
           05  SBLIM-MATURITY-DATE         PIC 9(08).
           05  SBLIM-NEXT-REVIEW-DATE      PIC 9(08).
           05  SBLIM-SOURCE-CD             PIC X(01).
               88  SBLIM-SOURCE-CD-E               VALUE 'E'.
               88  SBLIM-SOURCE-CD-J               VALUE 'J'.
               88  SBLIM-SOURCE-CD-L               VALUE 'L'.
               88  SBLIM-SOURCE-CD-U               VALUE 'U'.
           05  SBLIM-REASON-CD             PIC X(02).
               88  SBLIM-REASON-CD-DG              VALUE 'DG'.
               88  SBLIM-REASON-CD-UK              VALUE 'UK'.
               88  SBLIM-REASON-CD-VH              VALUE 'VH'.
               88  SBLIM-REASON-CD-XD              VALUE 'XD'.
           05  SBLIM-ISSUE-TYPE-CD         PIC X(02).
               88  SBLIM-ISSUE-TYPE-CD-CW          VALUE 'CW'.
               88  SBLIM-ISSUE-TYPE-CD-DY          VALUE 'DY'.
               88  SBLIM-ISSUE-TYPE-CD-JA          VALUE 'JA'.
               88  SBLIM-ISSUE-TYPE-CD-KT          VALUE 'KT'.
               88  SBLIM-ISSUE-TYPE-CD-MF          VALUE 'MF'.
               88  SBLIM-ISSUE-TYPE-CD-SA          VALUE 'SA'.
           05  SBLIM-SERIES-CD             PIC X(02).
           05  SBLIM-OVERRIDE-FLAG         PIC X(01).
               88  SBLIM-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  SBLIM-PREFERRED-FLAG        PIC X(01).
               88  SBLIM-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  SBLIM-HOLD-FLAG             PIC X(01).
               88  SBLIM-HOLD-FLAG-YES             VALUE 'Y'.
           05  SBLIM-PAYMENT-CNT           PIC 9(07).
           05  SBLIM-EXCEPT-CNT            PIC 9(07).
           05  SBLIM-PENALTY-PCT           PIC 9(03)V9(06).
           05  SBLIM-FEE-PCT               PIC 9(03)V9(06).
           05  SBLIM-REF-NO                PIC X(16).
           05  SBLIM-ADDR                  PIC X(30).
           05  SBLIM-RETURN-CODE           PIC 9(02).
