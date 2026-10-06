      *================================================================*
      * COPYBOOK : SBFEEP                                              *
      * PURPOSE  : LINKAGE FOR SAVINGS BOND SERVICES RULE MODULE SBFEE *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-04-18 TWH  ORIGINAL                                       *
      * 1993-05-16 LMB  NEW REDEEM REPORT COLUMNS CHG3624              *
      * 1995-12-10 MKL  ACCRUAL STATUS CODES ADDED REQ3788             *
      * 1999-05-14 RJT  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-07-26 EWS  INTEREST CALCULATION CORRECTED PRB1766         *
      * 2000-05-27 TWH  ISSUE-PRICE NOTICE TEXT PER LEGAL CHG3209      *
      * 2006-11-01 PQR  TAXABLE CUTOFF MOVED INC4525                   *
      * 2008-01-22 TWH  AUDIT FINDING - INTEREST CONTROL TOTALS INC9329*
      *================================================================*
       01  SBFEE-PARMS.
           05  SBFEE-PURCHASE-AMT          PIC S9(13)V99 COMP-3.
           05  SBFEE-FACE-YTD              PIC S9(13)V99 COMP-3.
           05  SBFEE-REDEEM-YTD            PIC S9(13)V99 COMP-3.
           05  SBFEE-ACCRUAL-AMT           PIC S9(13)V99 COMP-3.
           05  SBFEE-INTEREST-YTD          PIC S9(13)V99 COMP-3.
           05  SBFEE-TAXABLE-AMT           PIC S9(13)V99 COMP-3.
           05  SBFEE-NOTICE-DATE           PIC 9(08).
           05  SBFEE-NEXT-DUE-DATE         PIC 9(08).
           05  SBFEE-OPEN-DATE             PIC 9(08).
           05  SBFEE-LAST-MAINT-DATE       PIC 9(08).
           05  SBFEE-REASON-CD             PIC X(02).
               88  SBFEE-REASON-CD-DG              VALUE 'DG'.
               88  SBFEE-REASON-CD-UK              VALUE 'UK'.
               88  SBFEE-REASON-CD-VH              VALUE 'VH'.
               88  SBFEE-REASON-CD-XD              VALUE 'XD'.
           05  SBFEE-STATUS-CD             PIC X(02).
               88  SBFEE-STATUS-CD-CS              VALUE 'CS'.
               88  SBFEE-STATUS-CD-HF              VALUE 'HF'.
               88  SBFEE-STATUS-CD-JZ              VALUE 'JZ'.
           05  SBFEE-BRANCH-CD             PIC X(02).
           05  SBFEE-ISSUE-TYPE-CD         PIC X(02).
               88  SBFEE-ISSUE-TYPE-CD-CW          VALUE 'CW'.
               88  SBFEE-ISSUE-TYPE-CD-DY          VALUE 'DY'.
               88  SBFEE-ISSUE-TYPE-CD-JA          VALUE 'JA'.
               88  SBFEE-ISSUE-TYPE-CD-KT          VALUE 'KT'.
               88  SBFEE-ISSUE-TYPE-CD-MF          VALUE 'MF'.
               88  SBFEE-ISSUE-TYPE-CD-SA          VALUE 'SA'.
           05  SBFEE-AUTO-RENEW-FLAG       PIC X(01).
               88  SBFEE-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  SBFEE-NOTICE-SENT-FLAG      PIC X(01).
               88  SBFEE-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  SBFEE-REVIEW-FLAG           PIC X(01).
               88  SBFEE-REVIEW-FLAG-YES           VALUE 'Y'.
           05  SBFEE-RETRY-CNT             PIC 9(07).
           05  SBFEE-EXCEPT-CNT            PIC 9(07).
           05  SBFEE-MARGIN                PIC 9(03)V9(06).
           05  SBFEE-PENALTY-PCT           PIC 9(03)V9(06).
           05  SBFEE-CONTACT               PIC X(20).
           05  SBFEE-DESC                  PIC X(30).
           05  SBFEE-RETURN-CODE           PIC 9(02).
