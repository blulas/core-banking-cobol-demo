      *================================================================*
      * COPYBOOK : FXNV1                                               *
      * PURPOSE  : FOREIGN BANKNOTES EXTRACT RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-12 LMB  ORIGINAL                                       *
      * 1997-12-26 LMB  NEW MARGIN REPORT COLUMNS INC1624              *
      * 2000-04-09 RJT  USD NOTICE TEXT PER LEGAL PRB8089              *
      * 2001-11-23 PQR  PERFORMANCE - USD READ AHEAD INC7749           *
      * 2001-10-04 GAF  SHIPPING FEE SCHEDULE UPDATE PRB7297           *
      * 2002-05-15 DPS  BUYBACK EXCEPTIONS TO OPS QUEUE INC5494        *
      * 2002-12-03 BHS  PERFORMANCE - MARGIN READ AHEAD REQ7605        *
      * 2004-07-16 JLM  MARGIN CUTOFF MOVED CHG7153                    *
      * 2004-02-24 PQR  BUYBACK ROUNDING - HALF UP CHG4944             *
      * 2005-08-27 BHS  AUDIT FINDING - USD CONTROL TOTALS INC1683     *
      *================================================================*
       01  FXN-H1-REC.
           05  FXNH1-REF-NO                PIC X(16).
           05  FXNH1-CONTACT               PIC X(20).
           05  FXNH1-DESC                  PIC X(30).
           05  FXNH1-CUST-ID               PIC X(10).
           05  FXNH1-ADDR                  PIC X(30).
           05  FXNH1-KEY                   PIC X(12).
           05  FXNH1-NAME                  PIC X(30).
           05  FXN-H1-AMOUNTS.
               10  FXNH1-USD-YTD           PIC S9(13)V99 COMP-3.
               10  FXNH1-USD-AMT           PIC S9(13)V99 COMP-3.
               10  FXNH1-FOREIGN-AMT       PIC S9(13)V99 COMP-3.
               10  FXNH1-SHIPPING-AMT      PIC S9(13)V99 COMP-3.
               10  FXNH1-BUYBACK-AMT       PIC S9(13)V99 COMP-3.
               10  FXNH1-BUYBACK-YTD       PIC S9(13)V99 COMP-3.
               10  FXNH1-MARGIN-YTD        PIC S9(13)V99 COMP-3.
               10  FXNH1-SHIPPING-YTD      PIC S9(13)V99 COMP-3.
           05  FXN-H1-DATES.
               10  FXNH1-CLOSE-DATE        PIC 9(08).
               10  FXNH1-LAST-PAY-DATE     PIC 9(08).
               10  FXNH1-OPEN-DATE         PIC 9(08).
               10  FXNH1-LAST-ACT-DATE     PIC 9(08).
               10  FXNH1-MATURITY-DATE     PIC 9(08).
               10  FXNH1-EXPIRY-DATE       PIC 9(08).
               10  FXNH1-NOTICE-DATE       PIC 9(08).
               10  FXNH1-LAST-MAINT-DATE   PIC 9(08).
               10  FXNH1-EFFECTIVE-DATE    PIC 9(08).
           05  FXN-H1-CODES.
               10  FXNH1-REVIEW-FLAG       PIC X(01).
                   88  FXNH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  FXNH1-BRANCH-CD         PIC X(03).
                   88  FXNH1-BRANCH-CD-APZ         VALUE 'APZ'.
                   88  FXNH1-BRANCH-CD-BBP         VALUE 'BBP'.
                   88  FXNH1-BRANCH-CD-ENL         VALUE 'ENL'.
                   88  FXNH1-BRANCH-CD-EZB         VALUE 'EZB'.
                   88  FXNH1-BRANCH-CD-KHV         VALUE 'KHV'.
                   88  FXNH1-BRANCH-CD-TDN         VALUE 'TDN'.
               10  FXNH1-OVERRIDE-FLAG     PIC X(01).
                   88  FXNH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  FXNH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  FXNH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  FXNH1-ORDER-TYPE-CD     PIC X(03).
                   88  FXNH1-ORDER-TYPE-CD-DTU     VALUE 'DTU'.
                   88  FXNH1-ORDER-TYPE-CD-KZV     VALUE 'KZV'.
                   88  FXNH1-ORDER-TYPE-CD-PHV     VALUE 'PHV'.
                   88  FXNH1-ORDER-TYPE-CD-PZZ     VALUE 'PZZ'.
                   88  FXNH1-ORDER-TYPE-CD-WMC     VALUE 'WMC'.
                   88  FXNH1-ORDER-TYPE-CD-XWF     VALUE 'XWF'.
               10  FXNH1-TYPE-CD           PIC X(03).
                   88  FXNH1-TYPE-CD-CFP           VALUE 'CFP'.
                   88  FXNH1-TYPE-CD-FFU           VALUE 'FFU'.
                   88  FXNH1-TYPE-CD-JNR           VALUE 'JNR'.
                   88  FXNH1-TYPE-CD-UGS           VALUE 'UGS'.
                   88  FXNH1-TYPE-CD-XUM           VALUE 'XUM'.
               10  FXNH1-STATUS-CD         PIC X(03).
                   88  FXNH1-STATUS-CD-CXH         VALUE 'CXH'.
                   88  FXNH1-STATUS-CD-ESS         VALUE 'ESS'.
                   88  FXNH1-STATUS-CD-XDE         VALUE 'XDE'.
                   88  FXNH1-STATUS-CD-XFD         VALUE 'XFD'.
               10  FXNH1-SOURCE-CD         PIC X(02).
               10  FXNH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  FXNH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  FXNH1-EXEMPT-FLAG       PIC X(01).
                   88  FXNH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  FXNH1-PREFERRED-FLAG    PIC X(01).
                   88  FXNH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  FXNH1-CURRENCY-CD       PIC X(01).
                   88  FXNH1-CURRENCY-CD-C         VALUE 'C'.
                   88  FXNH1-CURRENCY-CD-E         VALUE 'E'.
                   88  FXNH1-CURRENCY-CD-G         VALUE 'G'.
                   88  FXNH1-CURRENCY-CD-P         VALUE 'P'.
                   88  FXNH1-CURRENCY-CD-W         VALUE 'W'.
           05  FXN-H1-COUNTERS.
               10  FXNH1-PAYMENT-CNT       PIC 9(07).
               10  FXNH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  FXNH1-RATE              PIC 9(03)V9(06).
               10  FXNH1-CYCLE-CNT         PIC 9(07).
               10  FXNH1-NOTICE-CNT        PIC 9(07).
               10  FXNH1-MARGIN            PIC 9(03)V9(06).
               10  FXNH1-EXCEPT-CNT        PIC 9(07).
           05  FILLER                      PIC X(20).
