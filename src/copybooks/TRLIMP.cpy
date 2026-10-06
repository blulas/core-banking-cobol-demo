      *================================================================*
      * COPYBOOK : TRLIMP                                              *
      * PURPOSE  : LINKAGE FOR PERSONAL TRUST RULE MODULE TRLIM        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-06-22 CRP  ORIGINAL                                       *
      * 1986-11-07 DPS  DISTRIB CALCULATION CORRECTED CHG5854          *
      * 1988-10-28 EWS  TRUSTEE-FEE CUTOFF MOVED REQ3783               *
      * 1991-08-06 GAF  MARKET EXCEPTIONS TO OPS QUEUE CHG4385         *
      * 2001-02-15 DPS  AUDIT FINDING - INCOME CONTROL TOTALS INC9203  *
      * 2002-12-25 RJT  AUDIT FINDING - INCOME CONTROL TOTALS REQ4483  *
      * 2002-05-09 CRP  TRUSTEE-FEE STATUS CODES ADDED CHG5314         *
      * 2006-07-14 LMB  NEW DISTRIB REPORT COLUMNS CHG1740             *
      *================================================================*
       01  TRLIM-PARMS.
           05  TRLIM-TRUSTEE-FEE-AMT       PIC S9(13)V99 COMP-3.
           05  TRLIM-DISTRIB-YTD           PIC S9(13)V99 COMP-3.
           05  TRLIM-DISTRIB-AMT           PIC S9(13)V99 COMP-3.
           05  TRLIM-INCOME-YTD            PIC S9(13)V99 COMP-3.
           05  TRLIM-INCOME-AMT            PIC S9(13)V99 COMP-3.
           05  TRLIM-PRINCIPAL-YTD         PIC S9(13)V99 COMP-3.
           05  TRLIM-CLOSE-DATE            PIC 9(08).
           05  TRLIM-LAST-MAINT-DATE       PIC 9(08).
           05  TRLIM-OPEN-DATE             PIC 9(08).
           05  TRLIM-NOTICE-DATE           PIC 9(08).
           05  TRLIM-CYCLE-CD              PIC X(02).
           05  TRLIM-TRUST-TYPE-CD         PIC X(03).
               88  TRLIM-TRUST-TYPE-CD-FLJ         VALUE 'FLJ'.
               88  TRLIM-TRUST-TYPE-CD-MNG         VALUE 'MNG'.
               88  TRLIM-TRUST-TYPE-CD-VFN         VALUE 'VFN'.
               88  TRLIM-TRUST-TYPE-CD-XPL         VALUE 'XPL'.
           05  TRLIM-REASON-CD             PIC X(01).
               88  TRLIM-REASON-CD-H               VALUE 'H'.
               88  TRLIM-REASON-CD-M               VALUE 'M'.
               88  TRLIM-REASON-CD-T               VALUE 'T'.
           05  TRLIM-STATUS-CD             PIC X(02).
               88  TRLIM-STATUS-CD-EH              VALUE 'EH'.
               88  TRLIM-STATUS-CD-GG              VALUE 'GG'.
               88  TRLIM-STATUS-CD-JG              VALUE 'JG'.
           05  TRLIM-STOP-FLAG             PIC X(01).
               88  TRLIM-STOP-FLAG-YES             VALUE 'Y'.
           05  TRLIM-EXEMPT-FLAG           PIC X(01).
               88  TRLIM-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  TRLIM-SUSPEND-FLAG          PIC X(01).
               88  TRLIM-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  TRLIM-PAYMENT-CNT           PIC 9(07).
           05  TRLIM-RETRY-CNT             PIC 9(07).
           05  TRLIM-MARGIN                PIC 9(03)V9(06).
           05  TRLIM-RATE                  PIC 9(03)V9(06).
           05  TRLIM-NAME                  PIC X(30).
           05  TRLIM-CUST-ID               PIC X(10).
           05  TRLIM-RETURN-CODE           PIC 9(02).
