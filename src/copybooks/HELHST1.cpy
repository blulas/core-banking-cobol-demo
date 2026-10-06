      *================================================================*
      * COPYBOOK : HELHST1                                             *
      * PURPOSE  : HOME EQUITY LINES PRIOR VERSION RECORD LAYOUT       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-22 SKN  ORIGINAL                                       *
      * 1995-06-14 CRP  ANNUAL-FEE STATUS CODES ADDED CHG1620          *
      * 1998-09-24 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-11-05 DPS  LIMIT NOTICE TEXT PER LEGAL INC8873            *
      * 2000-11-12 SKN  LIMIT TABLE EXPANDED CHG0884                   *
      * 2000-09-07 CRP  AUDIT FINDING - MIN-PAY CONTROL TOTALS CHG1814 *
      * 2001-06-03 BHS  INTEREST TABLE EXPANDED REQ5839                *
      * 2005-09-25 RJT  MIN-PAY TABLE EXPANDED INC7469                 *
      * 2005-11-09 LMB  ANNUAL-FEE FEE SCHEDULE UPDATE INC7078         *
      * 2006-08-14 DPS  BALANCE STATUS CODES ADDED PRB7445             *
      * 2007-07-02 TWH  PERFORMANCE - ANNUAL-FEE READ AHEAD REQ8100    *
      * 2008-07-24 GAF  MIN-PAY EXCEPTIONS TO OPS QUEUE INC6566        *
      *================================================================*
       01  HEL-H1-REC.
           05  HELH1-CONTACT               PIC X(20).
           05  HELH1-DESC                  PIC X(30).
           05  HELH1-KEY                   PIC X(12).
           05  HELH1-NAME                  PIC X(30).
           05  HELH1-CUST-ID               PIC X(10).
           05  HELH1-REF-NO                PIC X(16).
           05  HELH1-ADDR                  PIC X(30).
           05  HEL-H1-AMOUNTS.
               10  HELH1-APPRAISE-AMT      PIC S9(13)V99 COMP-3.
               10  HELH1-LIMIT-YTD         PIC S9(13)V99 COMP-3.
               10  HELH1-INTEREST-YTD      PIC S9(13)V99 COMP-3.
               10  HELH1-DRAW-YTD          PIC S9(13)V99 COMP-3.
               10  HELH1-ANNUAL-FEE-AMT    PIC S9(13)V99 COMP-3.
               10  HELH1-MIN-PAY-AMT       PIC S9(13)V99 COMP-3.
               10  HELH1-ANNUAL-FEE-YTD    PIC S9(13)V99 COMP-3.
               10  HELH1-LIMIT-AMT         PIC S9(13)V99 COMP-3.
               10  HELH1-INTEREST-AMT      PIC S9(13)V99 COMP-3.
               10  HELH1-DRAW-AMT          PIC S9(13)V99 COMP-3.
               10  HELH1-APPRAISE-YTD      PIC S9(13)V99 COMP-3.
               10  HELH1-BALANCE-AMT       PIC S9(13)V99 COMP-3.
               10  HELH1-MIN-PAY-YTD       PIC S9(13)V99 COMP-3.
               10  HELH1-BALANCE-YTD       PIC S9(13)V99 COMP-3.
           05  HEL-H1-DATES.
               10  HELH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  HELH1-EXPIRY-DATE       PIC 9(08).
               10  HELH1-CLOSE-DATE        PIC 9(08).
               10  HELH1-LAST-PAY-DATE     PIC 9(08).
               10  HELH1-LAST-MAINT-DATE   PIC 9(08).
               10  HELH1-OPEN-DATE         PIC 9(08).
               10  HELH1-EFFECTIVE-DATE    PIC 9(08).
               10  HELH1-NEXT-DUE-DATE     PIC 9(08).
           05  HEL-H1-CODES.
               10  HELH1-REGION-CD         PIC X(03).
                   88  HELH1-REGION-CD-BSD         VALUE 'BSD'.
                   88  HELH1-REGION-CD-HKW         VALUE 'HKW'.
                   88  HELH1-REGION-CD-HPB         VALUE 'HPB'.
                   88  HELH1-REGION-CD-ULJ         VALUE 'ULJ'.
               10  HELH1-OVERRIDE-FLAG     PIC X(01).
                   88  HELH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  HELH1-TYPE-CD           PIC X(02).
                   88  HELH1-TYPE-CD-CD            VALUE 'CD'.
                   88  HELH1-TYPE-CD-JW            VALUE 'JW'.
                   88  HELH1-TYPE-CD-KV            VALUE 'KV'.
                   88  HELH1-TYPE-CD-WS            VALUE 'WS'.
                   88  HELH1-TYPE-CD-XL            VALUE 'XL'.
                   88  HELH1-TYPE-CD-ZV            VALUE 'ZV'.
               10  HELH1-HOLD-FLAG         PIC X(01).
                   88  HELH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  HELH1-STATUS-CD         PIC X(02).
                   88  HELH1-STATUS-CD-GR          VALUE 'GR'.
                   88  HELH1-STATUS-CD-JC          VALUE 'JC'.
                   88  HELH1-STATUS-CD-KH          VALUE 'KH'.
                   88  HELH1-STATUS-CD-KN          VALUE 'KN'.
                   88  HELH1-STATUS-CD-PC          VALUE 'PC'.
                   88  HELH1-STATUS-CD-UZ          VALUE 'UZ'.
               10  HELH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  HELH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  HELH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  HELH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  HELH1-LIEN-POS-CD       PIC X(03).
                   88  HELH1-LIEN-POS-CD-AXX       VALUE 'AXX'.
                   88  HELH1-LIEN-POS-CD-PVA       VALUE 'PVA'.
                   88  HELH1-LIEN-POS-CD-SFC       VALUE 'SFC'.
               10  HELH1-REASON-CD         PIC X(03).
                   88  HELH1-REASON-CD-KST         VALUE 'KST'.
                   88  HELH1-REASON-CD-NWS         VALUE 'NWS'.
                   88  HELH1-REASON-CD-ZJM         VALUE 'ZJM'.
               10  HELH1-PREFERRED-FLAG    PIC X(01).
                   88  HELH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  HELH1-DRAW-PERIOD-CD    PIC X(03).
                   88  HELH1-DRAW-PERIOD-CD-CCV    VALUE 'CCV'.
                   88  HELH1-DRAW-PERIOD-CD-JSU    VALUE 'JSU'.
                   88  HELH1-DRAW-PERIOD-CD-KXA    VALUE 'KXA'.
                   88  HELH1-DRAW-PERIOD-CD-VMR    VALUE 'VMR'.
                   88  HELH1-DRAW-PERIOD-CD-WLJ    VALUE 'WLJ'.
                   88  HELH1-DRAW-PERIOD-CD-XRA    VALUE 'XRA'.
           05  HEL-H1-COUNTERS.
               10  HELH1-EXCEPT-CNT        PIC 9(07).
               10  HELH1-PAYMENT-CNT       PIC 9(07).
               10  HELH1-MARGIN            PIC 9(03)V9(06).
               10  HELH1-RETRY-CNT         PIC 9(07).
               10  HELH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  HELH1-NOTICE-CNT        PIC 9(07).
               10  HELH1-DISCOUNT          PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
