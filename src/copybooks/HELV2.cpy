      *================================================================*
      * COPYBOOK : HELV2                                               *
      * PURPOSE  : HOME EQUITY LINES EXTRACT RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-26 LMB  ORIGINAL                                       *
      * 1999-06-17 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-04 TWH  LIMIT ROUNDING - HALF UP REQ4170               *
      * 2004-04-26 LMB  PERFORMANCE - BALANCE READ AHEAD CHG0805       *
      * 2004-11-12 LMB  NEW LIMIT REPORT COLUMNS PRB9543               *
      *================================================================*
       01  HEL-H2-REC.
           05  HELH2-NAME                  PIC X(30).
           05  HELH2-KEY                   PIC X(12).
           05  HELH2-CUST-ID               PIC X(10).
           05  HELH2-REF-NO                PIC X(16).
           05  HELH2-CONTACT               PIC X(20).
           05  HELH2-DESC                  PIC X(30).
           05  HELH2-ADDR                  PIC X(30).
           05  HEL-H2-AMOUNTS.
               10  HELH2-LIMIT-AMT         PIC S9(13)V99 COMP-3.
               10  HELH2-DRAW-AMT          PIC S9(13)V99 COMP-3.
               10  HELH2-BALANCE-YTD       PIC S9(13)V99 COMP-3.
               10  HELH2-DRAW-YTD          PIC S9(13)V99 COMP-3.
               10  HELH2-LIMIT-YTD         PIC S9(13)V99 COMP-3.
               10  HELH2-APPRAISE-YTD      PIC S9(13)V99 COMP-3.
               10  HELH2-INTEREST-YTD      PIC S9(13)V99 COMP-3.
               10  HELH2-MIN-PAY-YTD       PIC S9(13)V99 COMP-3.
               10  HELH2-BALANCE-AMT       PIC S9(13)V99 COMP-3.
               10  HELH2-INTEREST-AMT      PIC S9(13)V99 COMP-3.
               10  HELH2-ANNUAL-FEE-YTD    PIC S9(13)V99 COMP-3.
           05  HEL-H2-DATES.
               10  HELH2-EXPIRY-DATE       PIC 9(08).
               10  HELH2-OPEN-DATE         PIC 9(08).
               10  HELH2-MATURITY-DATE     PIC 9(08).
               10  HELH2-NEXT-DUE-DATE     PIC 9(08).
               10  HELH2-LAST-MAINT-DATE   PIC 9(08).
               10  HELH2-LAST-PAY-DATE     PIC 9(08).
               10  HELH2-CLOSE-DATE        PIC 9(08).
               10  HELH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  HELH2-EFFECTIVE-DATE    PIC 9(08).
           05  HEL-H2-CODES.
               10  HELH2-PREFERRED-FLAG    PIC X(01).
                   88  HELH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  HELH2-SUSPEND-FLAG      PIC X(01).
                   88  HELH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  HELH2-AUTO-RENEW-FLAG   PIC X(01).
                   88  HELH2-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  HELH2-DRAW-PERIOD-CD    PIC X(03).
                   88  HELH2-DRAW-PERIOD-CD-CCV    VALUE 'CCV'.
                   88  HELH2-DRAW-PERIOD-CD-JSU    VALUE 'JSU'.
                   88  HELH2-DRAW-PERIOD-CD-KXA    VALUE 'KXA'.
                   88  HELH2-DRAW-PERIOD-CD-VMR    VALUE 'VMR'.
                   88  HELH2-DRAW-PERIOD-CD-WLJ    VALUE 'WLJ'.
                   88  HELH2-DRAW-PERIOD-CD-XRA    VALUE 'XRA'.
               10  HELH2-OVERRIDE-FLAG     PIC X(01).
                   88  HELH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  HELH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  HELH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  HELH2-REGION-CD         PIC X(03).
                   88  HELH2-REGION-CD-BSD         VALUE 'BSD'.
                   88  HELH2-REGION-CD-HKW         VALUE 'HKW'.
                   88  HELH2-REGION-CD-HPB         VALUE 'HPB'.
                   88  HELH2-REGION-CD-ULJ         VALUE 'ULJ'.
               10  HELH2-LIEN-POS-CD       PIC X(03).
                   88  HELH2-LIEN-POS-CD-AXX       VALUE 'AXX'.
                   88  HELH2-LIEN-POS-CD-PVA       VALUE 'PVA'.
                   88  HELH2-LIEN-POS-CD-SFC       VALUE 'SFC'.
               10  HELH2-REASON-CD         PIC X(03).
                   88  HELH2-REASON-CD-KST         VALUE 'KST'.
                   88  HELH2-REASON-CD-NWS         VALUE 'NWS'.
                   88  HELH2-REASON-CD-ZJM         VALUE 'ZJM'.
               10  HELH2-TYPE-CD           PIC X(02).
                   88  HELH2-TYPE-CD-CD            VALUE 'CD'.
                   88  HELH2-TYPE-CD-JW            VALUE 'JW'.
                   88  HELH2-TYPE-CD-KV            VALUE 'KV'.
                   88  HELH2-TYPE-CD-WS            VALUE 'WS'.
                   88  HELH2-TYPE-CD-XL            VALUE 'XL'.
                   88  HELH2-TYPE-CD-ZV            VALUE 'ZV'.
           05  HEL-H2-COUNTERS.
               10  HELH2-PENALTY-PCT       PIC 9(03)V9(06).
               10  HELH2-NOTICE-CNT        PIC 9(07).
               10  HELH2-RETRY-CNT         PIC 9(07).
               10  HELH2-MARGIN            PIC 9(03)V9(06).
               10  HELH2-EXCEPT-CNT        PIC 9(07).
               10  HELH2-PAYMENT-CNT       PIC 9(07).
               10  HELH2-DISCOUNT          PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
