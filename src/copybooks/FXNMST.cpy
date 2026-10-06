      *================================================================*
      * COPYBOOK : FXNMST                                              *
      * PURPOSE  : FOREIGN BANKNOTES MASTER RECORD (FXNMST KSDS)       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-11-28 LMB  ORIGINAL                                       *
      * 1998-09-22 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-15 PQR  USD LIMIT REVISED PRB9173                      *
      * 2000-10-12 RJT  MARGIN EXCEPTIONS TO OPS QUEUE INC5033         *
      * 2001-11-04 JLM  MARGIN CUTOFF MOVED CHG0192                    *
      * 2005-05-01 PQR  USD NOTICE TEXT PER LEGAL PRB4841              *
      *================================================================*
       01  FXN-MASTER-REC.
           05  FXNM-KEY                    PIC X(12).
           05  FXNM-CUST-ID                PIC X(10).
           05  FXNM-NAME                   PIC X(30).
           05  FXNM-DESC                   PIC X(30).
           05  FXNM-REF-NO                 PIC X(16).
           05  FXNM-ADDR                   PIC X(30).
           05  FXNM-CONTACT                PIC X(20).
           05  FXN-MASTER-AMOUNTS.
               10  FXNM-FOREIGN-AMT        PIC S9(13)V99 COMP-3.
               10  FXNM-FOREIGN-YTD        PIC S9(13)V99 COMP-3.
               10  FXNM-USD-AMT            PIC S9(13)V99 COMP-3.
               10  FXNM-USD-YTD            PIC S9(13)V99 COMP-3.
               10  FXNM-MARGIN-AMT         PIC S9(13)V99 COMP-3.
               10  FXNM-MARGIN-YTD         PIC S9(13)V99 COMP-3.
               10  FXNM-BUYBACK-AMT        PIC S9(13)V99 COMP-3.
               10  FXNM-BUYBACK-YTD        PIC S9(13)V99 COMP-3.
               10  FXNM-SHIPPING-AMT       PIC S9(13)V99 COMP-3.
               10  FXNM-SHIPPING-YTD       PIC S9(13)V99 COMP-3.
           05  FXN-MASTER-DATES.
               10  FXNM-CLOSE-DATE         PIC 9(08).
               10  FXNM-LAST-PAY-DATE      PIC 9(08).
               10  FXNM-EFFECTIVE-DATE     PIC 9(08).
               10  FXNM-LAST-MAINT-DATE    PIC 9(08).
               10  FXNM-LAST-ACT-DATE      PIC 9(08).
               10  FXNM-NOTICE-DATE        PIC 9(08).
               10  FXNM-OPEN-DATE          PIC 9(08).
               10  FXNM-EXPIRY-DATE        PIC 9(08).
               10  FXNM-MATURITY-DATE      PIC 9(08).
           05  FXN-MASTER-CODES.
               10  FXNM-CURRENCY-CD        PIC X(01).
                   88  FXNM-CURRENCY-CD-C          VALUE 'C'.
                   88  FXNM-CURRENCY-CD-E          VALUE 'E'.
                   88  FXNM-CURRENCY-CD-G          VALUE 'G'.
                   88  FXNM-CURRENCY-CD-P          VALUE 'P'.
                   88  FXNM-CURRENCY-CD-W          VALUE 'W'.
               10  FXNM-ORDER-TYPE-CD      PIC X(03).
                   88  FXNM-ORDER-TYPE-CD-DTU      VALUE 'DTU'.
                   88  FXNM-ORDER-TYPE-CD-KZV      VALUE 'KZV'.
                   88  FXNM-ORDER-TYPE-CD-PHV      VALUE 'PHV'.
                   88  FXNM-ORDER-TYPE-CD-PZZ      VALUE 'PZZ'.
                   88  FXNM-ORDER-TYPE-CD-WMC      VALUE 'WMC'.
                   88  FXNM-ORDER-TYPE-CD-XWF      VALUE 'XWF'.
               10  FXNM-TYPE-CD            PIC X(03).
                   88  FXNM-TYPE-CD-CFP            VALUE 'CFP'.
                   88  FXNM-TYPE-CD-FFU            VALUE 'FFU'.
                   88  FXNM-TYPE-CD-JNR            VALUE 'JNR'.
                   88  FXNM-TYPE-CD-UGS            VALUE 'UGS'.
                   88  FXNM-TYPE-CD-XUM            VALUE 'XUM'.
               10  FXNM-BRANCH-CD          PIC X(03).
                   88  FXNM-BRANCH-CD-APZ          VALUE 'APZ'.
                   88  FXNM-BRANCH-CD-BBP          VALUE 'BBP'.
                   88  FXNM-BRANCH-CD-ENL          VALUE 'ENL'.
                   88  FXNM-BRANCH-CD-EZB          VALUE 'EZB'.
                   88  FXNM-BRANCH-CD-KHV          VALUE 'KHV'.
                   88  FXNM-BRANCH-CD-TDN          VALUE 'TDN'.
               10  FXNM-STATUS-CD          PIC X(03).
                   88  FXNM-STATUS-CD-CXH          VALUE 'CXH'.
                   88  FXNM-STATUS-CD-ESS          VALUE 'ESS'.
                   88  FXNM-STATUS-CD-XDE          VALUE 'XDE'.
                   88  FXNM-STATUS-CD-XFD          VALUE 'XFD'.
               10  FXNM-SOURCE-CD          PIC X(02).
               10  FXNM-EXEMPT-FLAG        PIC X(01).
                   88  FXNM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  FXNM-PREFERRED-FLAG     PIC X(01).
                   88  FXNM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  FXNM-AUTO-RENEW-FLAG    PIC X(01).
                   88  FXNM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  FXNM-REVIEW-FLAG        PIC X(01).
                   88  FXNM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  FXNM-NOTICE-SENT-FLAG   PIC X(01).
                   88  FXNM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  FXNM-OVERRIDE-FLAG      PIC X(01).
                   88  FXNM-OVERRIDE-FLAG-YES      VALUE 'Y'.
           05  FXN-MASTER-COUNTERS.
               10  FXNM-EXCEPT-CNT         PIC 9(07).
               10  FXNM-NOTICE-CNT         PIC 9(07).
               10  FXNM-CYCLE-CNT          PIC 9(07).
               10  FXNM-PAYMENT-CNT        PIC 9(07).
               10  FXNM-RATE               PIC 9(03)V9(06).
               10  FXNM-MARGIN             PIC 9(03)V9(06).
               10  FXNM-PENALTY-PCT        PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
