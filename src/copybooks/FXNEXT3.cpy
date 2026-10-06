      *================================================================*
      * COPYBOOK : FXNEXT3                                             *
      * PURPOSE  : FOREIGN BANKNOTES HISTORY RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-01-15 EWS  ORIGINAL                                       *
      * 1997-06-14 BHS  MARGIN STATUS CODES ADDED INC5966              *
      * 1998-08-14 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-04-16 MKL  USD CALCULATION CORRECTED CHG3916              *
      * 2000-01-03 CRP  MARGIN ROUNDING - HALF UP REQ7188              *
      * 2003-02-22 SKN  SHIPPING EXCEPTIONS TO OPS QUEUE PRB4402       *
      * 2003-06-23 MKL  MARGIN STATUS CODES ADDED CHG8824              *
      * 2005-08-08 LMB  FOREIGN ROUNDING - HALF UP PRB0880             *
      *================================================================*
       01  FXN-H3-REC.
           05  FXNH3-DESC                  PIC X(30).
           05  FXNH3-CUST-ID               PIC X(10).
           05  FXNH3-CONTACT               PIC X(20).
           05  FXNH3-NAME                  PIC X(30).
           05  FXNH3-KEY                   PIC X(12).
           05  FXNH3-ADDR                  PIC X(30).
           05  FXNH3-REF-NO                PIC X(16).
           05  FXN-H3-AMOUNTS.
               10  FXNH3-SHIPPING-AMT      PIC S9(13)V99 COMP-3.
               10  FXNH3-MARGIN-YTD        PIC S9(13)V99 COMP-3.
               10  FXNH3-USD-YTD           PIC S9(13)V99 COMP-3.
               10  FXNH3-SHIPPING-YTD      PIC S9(13)V99 COMP-3.
               10  FXNH3-MARGIN-AMT        PIC S9(13)V99 COMP-3.
               10  FXNH3-BUYBACK-AMT       PIC S9(13)V99 COMP-3.
               10  FXNH3-USD-AMT           PIC S9(13)V99 COMP-3.
               10  FXNH3-BUYBACK-YTD       PIC S9(13)V99 COMP-3.
               10  FXNH3-FOREIGN-YTD       PIC S9(13)V99 COMP-3.
               10  FXNH3-FOREIGN-AMT       PIC S9(13)V99 COMP-3.
           05  FXN-H3-DATES.
               10  FXNH3-LAST-MAINT-DATE   PIC 9(08).
               10  FXNH3-MATURITY-DATE     PIC 9(08).
               10  FXNH3-NOTICE-DATE       PIC 9(08).
               10  FXNH3-EXPIRY-DATE       PIC 9(08).
               10  FXNH3-EFFECTIVE-DATE    PIC 9(08).
               10  FXNH3-CLOSE-DATE        PIC 9(08).
               10  FXNH3-OPEN-DATE         PIC 9(08).
               10  FXNH3-LAST-ACT-DATE     PIC 9(08).
               10  FXNH3-LAST-PAY-DATE     PIC 9(08).
           05  FXN-H3-CODES.
               10  FXNH3-REVIEW-FLAG       PIC X(01).
                   88  FXNH3-REVIEW-FLAG-YES       VALUE 'Y'.
               10  FXNH3-NOTICE-SENT-FLAG  PIC X(01).
                   88  FXNH3-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  FXNH3-TYPE-CD           PIC X(03).
                   88  FXNH3-TYPE-CD-CFP           VALUE 'CFP'.
                   88  FXNH3-TYPE-CD-FFU           VALUE 'FFU'.
                   88  FXNH3-TYPE-CD-JNR           VALUE 'JNR'.
                   88  FXNH3-TYPE-CD-UGS           VALUE 'UGS'.
                   88  FXNH3-TYPE-CD-XUM           VALUE 'XUM'.
               10  FXNH3-BRANCH-CD         PIC X(03).
                   88  FXNH3-BRANCH-CD-APZ         VALUE 'APZ'.
                   88  FXNH3-BRANCH-CD-BBP         VALUE 'BBP'.
                   88  FXNH3-BRANCH-CD-ENL         VALUE 'ENL'.
                   88  FXNH3-BRANCH-CD-EZB         VALUE 'EZB'.
                   88  FXNH3-BRANCH-CD-KHV         VALUE 'KHV'.
                   88  FXNH3-BRANCH-CD-TDN         VALUE 'TDN'.
               10  FXNH3-AUTO-RENEW-FLAG   PIC X(01).
                   88  FXNH3-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  FXNH3-ORDER-TYPE-CD     PIC X(03).
                   88  FXNH3-ORDER-TYPE-CD-DTU     VALUE 'DTU'.
                   88  FXNH3-ORDER-TYPE-CD-KZV     VALUE 'KZV'.
                   88  FXNH3-ORDER-TYPE-CD-PHV     VALUE 'PHV'.
                   88  FXNH3-ORDER-TYPE-CD-PZZ     VALUE 'PZZ'.
                   88  FXNH3-ORDER-TYPE-CD-WMC     VALUE 'WMC'.
                   88  FXNH3-ORDER-TYPE-CD-XWF     VALUE 'XWF'.
               10  FXNH3-PREFERRED-FLAG    PIC X(01).
                   88  FXNH3-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  FXNH3-STATUS-CD         PIC X(03).
                   88  FXNH3-STATUS-CD-CXH         VALUE 'CXH'.
                   88  FXNH3-STATUS-CD-ESS         VALUE 'ESS'.
                   88  FXNH3-STATUS-CD-XDE         VALUE 'XDE'.
                   88  FXNH3-STATUS-CD-XFD         VALUE 'XFD'.
               10  FXNH3-SOURCE-CD         PIC X(02).
               10  FXNH3-OVERRIDE-FLAG     PIC X(01).
                   88  FXNH3-OVERRIDE-FLAG-YES     VALUE 'Y'.
           05  FXN-H3-COUNTERS.
               10  FXNH3-EXCEPT-CNT        PIC 9(07).
               10  FXNH3-CYCLE-CNT         PIC 9(07).
               10  FXNH3-NOTICE-CNT        PIC 9(07).
               10  FXNH3-MARGIN            PIC 9(03)V9(06).
               10  FXNH3-RATE              PIC 9(03)V9(06).
               10  FXNH3-PAYMENT-CNT       PIC 9(07).
               10  FXNH3-PENALTY-PCT       PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
