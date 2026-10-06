      *================================================================*
      * COPYBOOK : BPYMST                                              *
      * PURPOSE  : BILL PAYMENT SERVICE MASTER RECORD (BPYMST KSDS)    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-01-13 DPS  ORIGINAL                                       *
      * 1994-05-11 LMB  REFUND CUTOFF MOVED CHG9791                    *
      * 1997-05-05 LMB  NEW FEE REPORT COLUMNS CHG9664                 *
      * 1997-07-26 RJT  FEE FEE SCHEDULE UPDATE PRB0498                *
      * 1997-06-12 RJT  PAYMENT STATUS CODES ADDED CHG5987             *
      * 2000-10-18 TWH  FEE LIMIT REVISED REQ1204                      *
      * 2003-07-09 PQR  FEE FEE SCHEDULE UPDATE CHG0300                *
      * 2007-03-18 BHS  FEE CUTOFF MOVED PRB3919                       *
      * 2008-03-20 EWS  RETURNED STATUS CODES ADDED CHG7172            *
      *================================================================*
       01  BPY-MASTER-REC.
           05  BPYM-KEY                    PIC X(12).
           05  BPYM-CUST-ID                PIC X(10).
           05  BPYM-NAME                   PIC X(30).
           05  BPYM-DESC                   PIC X(30).
           05  BPYM-REF-NO                 PIC X(16).
           05  BPYM-ADDR                   PIC X(30).
           05  BPYM-CONTACT                PIC X(20).
           05  BPY-MASTER-AMOUNTS.
               10  BPYM-PAYMENT-AMT        PIC S9(13)V99 COMP-3.
               10  BPYM-PAYMENT-YTD        PIC S9(13)V99 COMP-3.
               10  BPYM-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  BPYM-FEE-YTD            PIC S9(13)V99 COMP-3.
               10  BPYM-RETURNED-AMT       PIC S9(13)V99 COMP-3.
               10  BPYM-RETURNED-YTD       PIC S9(13)V99 COMP-3.
               10  BPYM-REFUND-AMT         PIC S9(13)V99 COMP-3.
               10  BPYM-REFUND-YTD         PIC S9(13)V99 COMP-3.
           05  BPY-MASTER-DATES.
               10  BPYM-MATURITY-DATE      PIC 9(08).
               10  BPYM-NOTICE-DATE        PIC 9(08).
               10  BPYM-LAST-STMT-DATE     PIC 9(08).
               10  BPYM-EFFECTIVE-DATE     PIC 9(08).
               10  BPYM-OPEN-DATE          PIC 9(08).
               10  BPYM-CLOSE-DATE         PIC 9(08).
               10  BPYM-NEXT-DUE-DATE      PIC 9(08).
               10  BPYM-LAST-ACT-DATE      PIC 9(08).
               10  BPYM-EXPIRY-DATE        PIC 9(08).
           05  BPY-MASTER-CODES.
               10  BPYM-PAYEE-TYPE-CD      PIC X(02).
                   88  BPYM-PAYEE-TYPE-CD-BT       VALUE 'BT'.
                   88  BPYM-PAYEE-TYPE-CD-FR       VALUE 'FR'.
                   88  BPYM-PAYEE-TYPE-CD-LL       VALUE 'LL'.
                   88  BPYM-PAYEE-TYPE-CD-US       VALUE 'US'.
                   88  BPYM-PAYEE-TYPE-CD-XM       VALUE 'XM'.
                   88  BPYM-PAYEE-TYPE-CD-YE       VALUE 'YE'.
               10  BPYM-REMIT-METHOD-CD    PIC X(03).
               10  BPYM-REASON-CD          PIC X(02).
               10  BPYM-CYCLE-CD           PIC X(02).
               10  BPYM-STATUS-CD          PIC X(02).
                   88  BPYM-STATUS-CD-JX           VALUE 'JX'.
                   88  BPYM-STATUS-CD-LR           VALUE 'LR'.
                   88  BPYM-STATUS-CD-NZ           VALUE 'NZ'.
                   88  BPYM-STATUS-CD-RV           VALUE 'RV'.
                   88  BPYM-STATUS-CD-TM           VALUE 'TM'.
                   88  BPYM-STATUS-CD-TZ           VALUE 'TZ'.
               10  BPYM-SOURCE-CD          PIC X(02).
               10  BPYM-OVERRIDE-FLAG      PIC X(01).
                   88  BPYM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  BPYM-AUTO-RENEW-FLAG    PIC X(01).
                   88  BPYM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  BPYM-SUSPEND-FLAG       PIC X(01).
                   88  BPYM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  BPYM-EXEMPT-FLAG        PIC X(01).
                   88  BPYM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  BPYM-PREFERRED-FLAG     PIC X(01).
                   88  BPYM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  BPYM-HOLD-FLAG          PIC X(01).
                   88  BPYM-HOLD-FLAG-YES          VALUE 'Y'.
           05  BPY-MASTER-COUNTERS.
               10  BPYM-PAYMENT-CNT        PIC 9(07).
               10  BPYM-CYCLE-CNT          PIC 9(07).
               10  BPYM-ITEM-CNT           PIC 9(07).
               10  BPYM-EXCEPT-CNT         PIC 9(07).
               10  BPYM-RATE               PIC 9(03)V9(06).
               10  BPYM-PENALTY-PCT        PIC 9(03)V9(06).
               10  BPYM-MARGIN             PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
