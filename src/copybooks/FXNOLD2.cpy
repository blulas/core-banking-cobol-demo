      *================================================================*
      * COPYBOOK : FXNOLD2                                             *
      * PURPOSE  : FOREIGN BANKNOTES PRIOR VERSION RECORD LAYOUT       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-07-15 PQR  ORIGINAL                                       *
      * 1995-01-15 DPS  SHIPPING ROUNDING - HALF UP REQ3122            *
      * 1997-12-24 BHS  USD LIMIT REVISED INC5325                      *
      * 1997-12-12 DPS  FOREIGN ROUNDING - HALF UP CHG2627             *
      * 1997-11-21 CRP  FOREIGN CALCULATION CORRECTED INC4468          *
      * 1998-07-27 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-07 SKN  USD STATUS CODES ADDED CHG3917                 *
      * 2002-05-16 GAF  FOREIGN CUTOFF MOVED REQ1509                   *
      * 2002-12-24 JLM  USD STATUS CODES ADDED PRB7762                 *
      * 2003-11-04 PQR  USD EXCEPTIONS TO OPS QUEUE PRB7489            *
      * 2004-12-28 CRP  AUDIT FINDING - MARGIN CONTROL TOTALS INC0868  *
      *================================================================*
       01  FXN-H2-REC.
           05  FXNH2-DESC                  PIC X(30).
           05  FXNH2-CONTACT               PIC X(20).
           05  FXNH2-CUST-ID               PIC X(10).
           05  FXNH2-NAME                  PIC X(30).
           05  FXNH2-REF-NO                PIC X(16).
           05  FXNH2-ADDR                  PIC X(30).
           05  FXNH2-KEY                   PIC X(12).
           05  FXN-H2-AMOUNTS.
               10  FXNH2-MARGIN-YTD        PIC S9(13)V99 COMP-3.
               10  FXNH2-FOREIGN-YTD       PIC S9(13)V99 COMP-3.
               10  FXNH2-SHIPPING-AMT      PIC S9(13)V99 COMP-3.
               10  FXNH2-USD-YTD           PIC S9(13)V99 COMP-3.
               10  FXNH2-SHIPPING-YTD      PIC S9(13)V99 COMP-3.
               10  FXNH2-FOREIGN-AMT       PIC S9(13)V99 COMP-3.
               10  FXNH2-USD-AMT           PIC S9(13)V99 COMP-3.
               10  FXNH2-BUYBACK-YTD       PIC S9(13)V99 COMP-3.
               10  FXNH2-BUYBACK-AMT       PIC S9(13)V99 COMP-3.
               10  FXNH2-MARGIN-AMT        PIC S9(13)V99 COMP-3.
           05  FXN-H2-DATES.
               10  FXNH2-MATURITY-DATE     PIC 9(08).
               10  FXNH2-LAST-MAINT-DATE   PIC 9(08).
               10  FXNH2-EFFECTIVE-DATE    PIC 9(08).
               10  FXNH2-CLOSE-DATE        PIC 9(08).
               10  FXNH2-EXPIRY-DATE       PIC 9(08).
               10  FXNH2-OPEN-DATE         PIC 9(08).
               10  FXNH2-NOTICE-DATE       PIC 9(08).
               10  FXNH2-LAST-ACT-DATE     PIC 9(08).
           05  FXN-H2-CODES.
               10  FXNH2-PREFERRED-FLAG    PIC X(01).
                   88  FXNH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  FXNH2-REVIEW-FLAG       PIC X(01).
                   88  FXNH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  FXNH2-STATUS-CD         PIC X(03).
                   88  FXNH2-STATUS-CD-CXH         VALUE 'CXH'.
                   88  FXNH2-STATUS-CD-ESS         VALUE 'ESS'.
                   88  FXNH2-STATUS-CD-XDE         VALUE 'XDE'.
                   88  FXNH2-STATUS-CD-XFD         VALUE 'XFD'.
               10  FXNH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  FXNH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  FXNH2-BRANCH-CD         PIC X(03).
                   88  FXNH2-BRANCH-CD-APZ         VALUE 'APZ'.
                   88  FXNH2-BRANCH-CD-BBP         VALUE 'BBP'.
                   88  FXNH2-BRANCH-CD-ENL         VALUE 'ENL'.
                   88  FXNH2-BRANCH-CD-EZB         VALUE 'EZB'.
                   88  FXNH2-BRANCH-CD-KHV         VALUE 'KHV'.
                   88  FXNH2-BRANCH-CD-TDN         VALUE 'TDN'.
               10  FXNH2-ORDER-TYPE-CD     PIC X(03).
                   88  FXNH2-ORDER-TYPE-CD-DTU     VALUE 'DTU'.
                   88  FXNH2-ORDER-TYPE-CD-KZV     VALUE 'KZV'.
                   88  FXNH2-ORDER-TYPE-CD-PHV     VALUE 'PHV'.
                   88  FXNH2-ORDER-TYPE-CD-PZZ     VALUE 'PZZ'.
                   88  FXNH2-ORDER-TYPE-CD-WMC     VALUE 'WMC'.
                   88  FXNH2-ORDER-TYPE-CD-XWF     VALUE 'XWF'.
               10  FXNH2-TYPE-CD           PIC X(03).
                   88  FXNH2-TYPE-CD-CFP           VALUE 'CFP'.
                   88  FXNH2-TYPE-CD-FFU           VALUE 'FFU'.
                   88  FXNH2-TYPE-CD-JNR           VALUE 'JNR'.
                   88  FXNH2-TYPE-CD-UGS           VALUE 'UGS'.
                   88  FXNH2-TYPE-CD-XUM           VALUE 'XUM'.
               10  FXNH2-SOURCE-CD         PIC X(02).
               10  FXNH2-OVERRIDE-FLAG     PIC X(01).
                   88  FXNH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  FXNH2-AUTO-RENEW-FLAG   PIC X(01).
                   88  FXNH2-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  FXNH2-CURRENCY-CD       PIC X(01).
                   88  FXNH2-CURRENCY-CD-C         VALUE 'C'.
                   88  FXNH2-CURRENCY-CD-E         VALUE 'E'.
                   88  FXNH2-CURRENCY-CD-G         VALUE 'G'.
                   88  FXNH2-CURRENCY-CD-P         VALUE 'P'.
                   88  FXNH2-CURRENCY-CD-W         VALUE 'W'.
           05  FXN-H2-COUNTERS.
               10  FXNH2-MARGIN            PIC 9(03)V9(06).
               10  FXNH2-PENALTY-PCT       PIC 9(03)V9(06).
               10  FXNH2-NOTICE-CNT        PIC 9(07).
               10  FXNH2-PAYMENT-CNT       PIC 9(07).
               10  FXNH2-EXCEPT-CNT        PIC 9(07).
           05  FILLER                      PIC X(20).
