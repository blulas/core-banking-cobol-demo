      *================================================================*
      * COPYBOOK : PBKMST                                              *
      * PURPOSE  : PASSBOOK PRINTING MASTER RECORD (PBKMST KSDS)       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-08-14 DPS  ORIGINAL                                       *
      * 1996-03-20 EWS  AUDIT FINDING - LINE CONTROL TOTALS REQ5372    *
      * 1996-11-28 EWS  UNPOSTED FEE SCHEDULE UPDATE PRB5204           *
      * 1998-02-01 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-02-12 LMB  UNPOSTED TABLE EXPANDED REQ9268                *
      * 2001-07-01 MKL  BALANCE EXCEPTIONS TO OPS QUEUE INC4943        *
      * 2001-02-05 JLM  CARRYFWD TABLE EXPANDED INC7733                *
      * 2003-09-11 EWS  UNPOSTED CUTOFF MOVED PRB7291                  *
      * 2005-05-23 PQR  UNPOSTED ROUNDING - HALF UP PRB4559            *
      * 2007-10-15 CRP  INTEREST EXCEPTIONS TO OPS QUEUE INC1466       *
      * 2008-01-07 RJT  BALANCE FEE SCHEDULE UPDATE PRB4770            *
      * 2011-05-27 LMB  NEW LINE REPORT COLUMNS CHG9833                *
      * 2012-08-12 PQR  UNPOSTED CALCULATION CORRECTED REQ3359         *
      *================================================================*
       01  PBK-MASTER-REC.
           05  PBKM-KEY                    PIC X(12).
           05  PBKM-CUST-ID                PIC X(10).
           05  PBKM-NAME                   PIC X(30).
           05  PBKM-DESC                   PIC X(30).
           05  PBKM-REF-NO                 PIC X(16).
           05  PBKM-ADDR                   PIC X(30).
           05  PBKM-CONTACT                PIC X(20).
           05  PBK-MASTER-AMOUNTS.
               10  PBKM-LINE-AMT           PIC S9(13)V99 COMP-3.
               10  PBKM-LINE-YTD           PIC S9(13)V99 COMP-3.
               10  PBKM-UNPOSTED-AMT       PIC S9(13)V99 COMP-3.
               10  PBKM-UNPOSTED-YTD       PIC S9(13)V99 COMP-3.
               10  PBKM-PRINTED-AMT        PIC S9(13)V99 COMP-3.
               10  PBKM-PRINTED-YTD        PIC S9(13)V99 COMP-3.
               10  PBKM-CARRYFWD-AMT       PIC S9(13)V99 COMP-3.
               10  PBKM-CARRYFWD-YTD       PIC S9(13)V99 COMP-3.
               10  PBKM-INTEREST-AMT       PIC S9(13)V99 COMP-3.
               10  PBKM-INTEREST-YTD       PIC S9(13)V99 COMP-3.
               10  PBKM-BALANCE-AMT        PIC S9(13)V99 COMP-3.
               10  PBKM-BALANCE-YTD        PIC S9(13)V99 COMP-3.
           05  PBK-MASTER-DATES.
               10  PBKM-MATURITY-DATE      PIC 9(08).
               10  PBKM-CLOSE-DATE         PIC 9(08).
               10  PBKM-LAST-MAINT-DATE    PIC 9(08).
               10  PBKM-EFFECTIVE-DATE     PIC 9(08).
               10  PBKM-LAST-ACT-DATE      PIC 9(08).
               10  PBKM-EXPIRY-DATE        PIC 9(08).
               10  PBKM-NEXT-DUE-DATE      PIC 9(08).
               10  PBKM-NEXT-REVIEW-DATE   PIC 9(08).
               10  PBKM-OPEN-DATE          PIC 9(08).
           05  PBK-MASTER-CODES.
               10  PBKM-BOOK-TYPE-CD       PIC X(02).
                   88  PBKM-BOOK-TYPE-CD-CH        VALUE 'CH'.
                   88  PBKM-BOOK-TYPE-CD-HG        VALUE 'HG'.
                   88  PBKM-BOOK-TYPE-CD-KB        VALUE 'KB'.
                   88  PBKM-BOOK-TYPE-CD-KW        VALUE 'KW'.
                   88  PBKM-BOOK-TYPE-CD-PA        VALUE 'PA'.
                   88  PBKM-BOOK-TYPE-CD-TR        VALUE 'TR'.
               10  PBKM-PRINTER-CD         PIC X(02).
               10  PBKM-BRANCH-CD          PIC X(02).
                   88  PBKM-BRANCH-CD-DF           VALUE 'DF'.
                   88  PBKM-BRANCH-CD-NV           VALUE 'NV'.
                   88  PBKM-BRANCH-CD-PP           VALUE 'PP'.
                   88  PBKM-BRANCH-CD-SH           VALUE 'SH'.
                   88  PBKM-BRANCH-CD-ZP           VALUE 'ZP'.
               10  PBKM-SOURCE-CD          PIC X(02).
                   88  PBKM-SOURCE-CD-JX           VALUE 'JX'.
                   88  PBKM-SOURCE-CD-ZC           VALUE 'ZC'.
                   88  PBKM-SOURCE-CD-ZY           VALUE 'ZY'.
               10  PBKM-CHANNEL-CD         PIC X(01).
                   88  PBKM-CHANNEL-CD-B           VALUE 'B'.
                   88  PBKM-CHANNEL-CD-D           VALUE 'D'.
                   88  PBKM-CHANNEL-CD-H           VALUE 'H'.
                   88  PBKM-CHANNEL-CD-L           VALUE 'L'.
                   88  PBKM-CHANNEL-CD-M           VALUE 'M'.
                   88  PBKM-CHANNEL-CD-W           VALUE 'W'.
               10  PBKM-REGION-CD          PIC X(02).
                   88  PBKM-REGION-CD-ER           VALUE 'ER'.
                   88  PBKM-REGION-CD-GL           VALUE 'GL'.
                   88  PBKM-REGION-CD-PE           VALUE 'PE'.
                   88  PBKM-REGION-CD-RJ           VALUE 'RJ'.
                   88  PBKM-REGION-CD-VB           VALUE 'VB'.
               10  PBKM-EXEMPT-FLAG        PIC X(01).
                   88  PBKM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  PBKM-NOTICE-SENT-FLAG   PIC X(01).
                   88  PBKM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  PBKM-AUTO-RENEW-FLAG    PIC X(01).
                   88  PBKM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  PBKM-SUSPEND-FLAG       PIC X(01).
                   88  PBKM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  PBKM-HOLD-FLAG          PIC X(01).
                   88  PBKM-HOLD-FLAG-YES          VALUE 'Y'.
               10  PBKM-REVIEW-FLAG        PIC X(01).
                   88  PBKM-REVIEW-FLAG-YES        VALUE 'Y'.
           05  PBK-MASTER-COUNTERS.
               10  PBKM-CYCLE-CNT          PIC 9(07).
               10  PBKM-RETRY-CNT          PIC 9(07).
               10  PBKM-PAYMENT-CNT        PIC 9(07).
               10  PBKM-ITEM-CNT           PIC 9(07).
               10  PBKM-PENALTY-PCT        PIC 9(03)V9(06).
               10  PBKM-MARGIN             PIC 9(03)V9(06).
               10  PBKM-FEE-PCT            PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
