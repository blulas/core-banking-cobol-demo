      *================================================================*
      * COPYBOOK : HELMST                                              *
      * PURPOSE  : HOME EQUITY LINES MASTER RECORD (HELMST KSDS)       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-08-26 TWH  ORIGINAL                                       *
      * 1999-07-27 JLM  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-05-14 SKN  AUDIT FINDING - DRAW CONTROL TOTALS PRB8377    *
      * 2004-02-28 RJT  INTEREST LIMIT REVISED INC2975                 *
      * 2005-12-06 CRP  DRAW FEE SCHEDULE UPDATE PRB9057               *
      *================================================================*
       01  HEL-MASTER-REC.
           05  HELM-KEY                    PIC X(12).
           05  HELM-CUST-ID                PIC X(10).
           05  HELM-NAME                   PIC X(30).
           05  HELM-DESC                   PIC X(30).
           05  HELM-REF-NO                 PIC X(16).
           05  HELM-ADDR                   PIC X(30).
           05  HELM-CONTACT                PIC X(20).
           05  HEL-MASTER-AMOUNTS.
               10  HELM-LIMIT-AMT          PIC S9(13)V99 COMP-3.
               10  HELM-LIMIT-YTD          PIC S9(13)V99 COMP-3.
               10  HELM-DRAW-AMT           PIC S9(13)V99 COMP-3.
               10  HELM-DRAW-YTD           PIC S9(13)V99 COMP-3.
               10  HELM-BALANCE-AMT        PIC S9(13)V99 COMP-3.
               10  HELM-BALANCE-YTD        PIC S9(13)V99 COMP-3.
               10  HELM-INTEREST-AMT       PIC S9(13)V99 COMP-3.
               10  HELM-INTEREST-YTD       PIC S9(13)V99 COMP-3.
               10  HELM-MIN-PAY-AMT        PIC S9(13)V99 COMP-3.
               10  HELM-MIN-PAY-YTD        PIC S9(13)V99 COMP-3.
               10  HELM-ANNUAL-FEE-AMT     PIC S9(13)V99 COMP-3.
               10  HELM-ANNUAL-FEE-YTD     PIC S9(13)V99 COMP-3.
               10  HELM-APPRAISE-AMT       PIC S9(13)V99 COMP-3.
               10  HELM-APPRAISE-YTD       PIC S9(13)V99 COMP-3.
           05  HEL-MASTER-DATES.
               10  HELM-OPEN-DATE          PIC 9(08).
               10  HELM-NEXT-REVIEW-DATE   PIC 9(08).
               10  HELM-NEXT-DUE-DATE      PIC 9(08).
               10  HELM-EXPIRY-DATE        PIC 9(08).
               10  HELM-EFFECTIVE-DATE     PIC 9(08).
               10  HELM-CLOSE-DATE         PIC 9(08).
               10  HELM-MATURITY-DATE      PIC 9(08).
               10  HELM-LAST-PAY-DATE      PIC 9(08).
               10  HELM-LAST-MAINT-DATE    PIC 9(08).
           05  HEL-MASTER-CODES.
               10  HELM-LIEN-POS-CD        PIC X(03).
                   88  HELM-LIEN-POS-CD-AXX        VALUE 'AXX'.
                   88  HELM-LIEN-POS-CD-PVA        VALUE 'PVA'.
                   88  HELM-LIEN-POS-CD-SFC        VALUE 'SFC'.
               10  HELM-DRAW-PERIOD-CD     PIC X(03).
                   88  HELM-DRAW-PERIOD-CD-CCV     VALUE 'CCV'.
                   88  HELM-DRAW-PERIOD-CD-JSU     VALUE 'JSU'.
                   88  HELM-DRAW-PERIOD-CD-KXA     VALUE 'KXA'.
                   88  HELM-DRAW-PERIOD-CD-VMR     VALUE 'VMR'.
                   88  HELM-DRAW-PERIOD-CD-WLJ     VALUE 'WLJ'.
                   88  HELM-DRAW-PERIOD-CD-XRA     VALUE 'XRA'.
               10  HELM-TYPE-CD            PIC X(02).
                   88  HELM-TYPE-CD-CD             VALUE 'CD'.
                   88  HELM-TYPE-CD-JW             VALUE 'JW'.
                   88  HELM-TYPE-CD-KV             VALUE 'KV'.
                   88  HELM-TYPE-CD-WS             VALUE 'WS'.
                   88  HELM-TYPE-CD-XL             VALUE 'XL'.
                   88  HELM-TYPE-CD-ZV             VALUE 'ZV'.
               10  HELM-REGION-CD          PIC X(03).
                   88  HELM-REGION-CD-BSD          VALUE 'BSD'.
                   88  HELM-REGION-CD-HKW          VALUE 'HKW'.
                   88  HELM-REGION-CD-HPB          VALUE 'HPB'.
                   88  HELM-REGION-CD-ULJ          VALUE 'ULJ'.
               10  HELM-STATUS-CD          PIC X(02).
                   88  HELM-STATUS-CD-GR           VALUE 'GR'.
                   88  HELM-STATUS-CD-JC           VALUE 'JC'.
                   88  HELM-STATUS-CD-KH           VALUE 'KH'.
                   88  HELM-STATUS-CD-KN           VALUE 'KN'.
                   88  HELM-STATUS-CD-PC           VALUE 'PC'.
                   88  HELM-STATUS-CD-UZ           VALUE 'UZ'.
               10  HELM-REASON-CD          PIC X(03).
                   88  HELM-REASON-CD-KST          VALUE 'KST'.
                   88  HELM-REASON-CD-NWS          VALUE 'NWS'.
                   88  HELM-REASON-CD-ZJM          VALUE 'ZJM'.
               10  HELM-SUSPEND-FLAG       PIC X(01).
                   88  HELM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  HELM-OVERRIDE-FLAG      PIC X(01).
                   88  HELM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  HELM-NOTICE-SENT-FLAG   PIC X(01).
                   88  HELM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  HELM-HOLD-FLAG          PIC X(01).
                   88  HELM-HOLD-FLAG-YES          VALUE 'Y'.
               10  HELM-AUTO-RENEW-FLAG    PIC X(01).
                   88  HELM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  HELM-PREFERRED-FLAG     PIC X(01).
                   88  HELM-PREFERRED-FLAG-YES     VALUE 'Y'.
           05  HEL-MASTER-COUNTERS.
               10  HELM-EXCEPT-CNT         PIC 9(07).
               10  HELM-RETRY-CNT          PIC 9(07).
               10  HELM-NOTICE-CNT         PIC 9(07).
               10  HELM-PAYMENT-CNT        PIC 9(07).
               10  HELM-PENALTY-PCT        PIC 9(03)V9(06).
               10  HELM-MARGIN             PIC 9(03)V9(06).
               10  HELM-DISCOUNT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
