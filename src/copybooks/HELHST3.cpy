      *================================================================*
      * COPYBOOK : HELHST3                                             *
      * PURPOSE  : HOME EQUITY LINES ARCHIVE RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-25 TWH  ORIGINAL                                       *
      * 2000-03-07 TWH  PERFORMANCE - MIN-PAY READ AHEAD INC1772       *
      * 2001-07-05 EWS  DRAW TABLE EXPANDED PRB6365                    *
      * 2003-12-22 MKL  ANNUAL-FEE NOTICE TEXT PER LEGAL PRB7829       *
      * 2004-12-20 DPS  PERFORMANCE - INTEREST READ AHEAD CHG5307      *
      * 2004-01-26 JLM  ANNUAL-FEE EXCEPTIONS TO OPS QUEUE INC0111     *
      * 2004-12-14 LMB  MIN-PAY STATUS CODES ADDED REQ1626             *
      *================================================================*
       01  HEL-H3-REC.
           05  HELH3-CUST-ID               PIC X(10).
           05  HELH3-NAME                  PIC X(30).
           05  HELH3-KEY                   PIC X(12).
           05  HELH3-CONTACT               PIC X(20).
           05  HELH3-REF-NO                PIC X(16).
           05  HELH3-DESC                  PIC X(30).
           05  HELH3-ADDR                  PIC X(30).
           05  HEL-H3-AMOUNTS.
               10  HELH3-MIN-PAY-AMT       PIC S9(13)V99 COMP-3.
               10  HELH3-BALANCE-YTD       PIC S9(13)V99 COMP-3.
               10  HELH3-APPRAISE-AMT      PIC S9(13)V99 COMP-3.
               10  HELH3-ANNUAL-FEE-AMT    PIC S9(13)V99 COMP-3.
               10  HELH3-LIMIT-YTD         PIC S9(13)V99 COMP-3.
               10  HELH3-DRAW-YTD          PIC S9(13)V99 COMP-3.
               10  HELH3-MIN-PAY-YTD       PIC S9(13)V99 COMP-3.
               10  HELH3-BALANCE-AMT       PIC S9(13)V99 COMP-3.
               10  HELH3-LIMIT-AMT         PIC S9(13)V99 COMP-3.
               10  HELH3-DRAW-AMT          PIC S9(13)V99 COMP-3.
               10  HELH3-ANNUAL-FEE-YTD    PIC S9(13)V99 COMP-3.
               10  HELH3-INTEREST-AMT      PIC S9(13)V99 COMP-3.
               10  HELH3-INTEREST-YTD      PIC S9(13)V99 COMP-3.
               10  HELH3-APPRAISE-YTD      PIC S9(13)V99 COMP-3.
           05  HEL-H3-DATES.
               10  HELH3-NEXT-REVIEW-DATE  PIC 9(08).
               10  HELH3-LAST-PAY-DATE     PIC 9(08).
               10  HELH3-EXPIRY-DATE       PIC 9(08).
               10  HELH3-EFFECTIVE-DATE    PIC 9(08).
               10  HELH3-MATURITY-DATE     PIC 9(08).
               10  HELH3-LAST-MAINT-DATE   PIC 9(08).
               10  HELH3-OPEN-DATE         PIC 9(08).
               10  HELH3-NEXT-DUE-DATE     PIC 9(08).
           05  HEL-H3-CODES.
               10  HELH3-REASON-CD         PIC X(03).
                   88  HELH3-REASON-CD-KST         VALUE 'KST'.
                   88  HELH3-REASON-CD-NWS         VALUE 'NWS'.
                   88  HELH3-REASON-CD-ZJM         VALUE 'ZJM'.
               10  HELH3-NOTICE-SENT-FLAG  PIC X(01).
                   88  HELH3-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  HELH3-DRAW-PERIOD-CD    PIC X(03).
                   88  HELH3-DRAW-PERIOD-CD-CCV    VALUE 'CCV'.
                   88  HELH3-DRAW-PERIOD-CD-JSU    VALUE 'JSU'.
                   88  HELH3-DRAW-PERIOD-CD-KXA    VALUE 'KXA'.
                   88  HELH3-DRAW-PERIOD-CD-VMR    VALUE 'VMR'.
                   88  HELH3-DRAW-PERIOD-CD-WLJ    VALUE 'WLJ'.
                   88  HELH3-DRAW-PERIOD-CD-XRA    VALUE 'XRA'.
               10  HELH3-AUTO-RENEW-FLAG   PIC X(01).
                   88  HELH3-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  HELH3-TYPE-CD           PIC X(02).
                   88  HELH3-TYPE-CD-CD            VALUE 'CD'.
                   88  HELH3-TYPE-CD-JW            VALUE 'JW'.
                   88  HELH3-TYPE-CD-KV            VALUE 'KV'.
                   88  HELH3-TYPE-CD-WS            VALUE 'WS'.
                   88  HELH3-TYPE-CD-XL            VALUE 'XL'.
                   88  HELH3-TYPE-CD-ZV            VALUE 'ZV'.
               10  HELH3-HOLD-FLAG         PIC X(01).
                   88  HELH3-HOLD-FLAG-YES         VALUE 'Y'.
               10  HELH3-STATUS-CD         PIC X(02).
                   88  HELH3-STATUS-CD-GR          VALUE 'GR'.
                   88  HELH3-STATUS-CD-JC          VALUE 'JC'.
                   88  HELH3-STATUS-CD-KH          VALUE 'KH'.
                   88  HELH3-STATUS-CD-KN          VALUE 'KN'.
                   88  HELH3-STATUS-CD-PC          VALUE 'PC'.
                   88  HELH3-STATUS-CD-UZ          VALUE 'UZ'.
               10  HELH3-PREFERRED-FLAG    PIC X(01).
                   88  HELH3-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  HELH3-SUSPEND-FLAG      PIC X(01).
                   88  HELH3-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  HELH3-LIEN-POS-CD       PIC X(03).
                   88  HELH3-LIEN-POS-CD-AXX       VALUE 'AXX'.
                   88  HELH3-LIEN-POS-CD-PVA       VALUE 'PVA'.
                   88  HELH3-LIEN-POS-CD-SFC       VALUE 'SFC'.
               10  HELH3-OVERRIDE-FLAG     PIC X(01).
                   88  HELH3-OVERRIDE-FLAG-YES     VALUE 'Y'.
           05  HEL-H3-COUNTERS.
               10  HELH3-RETRY-CNT         PIC 9(07).
               10  HELH3-MARGIN            PIC 9(03)V9(06).
               10  HELH3-DISCOUNT          PIC 9(03)V9(06).
               10  HELH3-PENALTY-PCT       PIC 9(03)V9(06).
               10  HELH3-EXCEPT-CNT        PIC 9(07).
               10  HELH3-PAYMENT-CNT       PIC 9(07).
               10  HELH3-NOTICE-CNT        PIC 9(07).
           05  FILLER                      PIC X(20).
