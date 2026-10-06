      *================================================================*
      * COPYBOOK : HESTATP                                             *
      * PURPOSE  : LINKAGE FOR HOME EQUITY LINES RULE MODULE HESTAT    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-17 CRP  ORIGINAL                                       *
      * 1995-10-03 JLM  ANNUAL-FEE EXCEPTIONS TO OPS QUEUE PRB8759     *
      * 1995-11-13 MKL  ANNUAL-FEE LIMIT REVISED CHG3178               *
      * 1997-01-10 EWS  AUDIT FINDING - APPRAISE CONTROL TOTALS REQ1313*
      * 1999-08-07 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-04-06 DPS  LIMIT EXCEPTIONS TO OPS QUEUE CHG2034          *
      * 2000-04-16 LMB  PERFORMANCE - DRAW READ AHEAD PRB3554          *
      * 2001-12-24 TWH  ANNUAL-FEE CALCULATION CORRECTED REQ4787       *
      * 2002-06-21 EWS  INTEREST RERUN PROTECTION PRB0302              *
      *================================================================*
       01  HESTAT-PARMS.
           05  HESTAT-DRAW-AMT             PIC S9(13)V99 COMP-3.
           05  HESTAT-ANNUAL-FEE-AMT       PIC S9(13)V99 COMP-3.
           05  HESTAT-APPRAISE-YTD         PIC S9(13)V99 COMP-3.
           05  HESTAT-BALANCE-AMT          PIC S9(13)V99 COMP-3.
           05  HESTAT-ANNUAL-FEE-YTD       PIC S9(13)V99 COMP-3.
           05  HESTAT-BALANCE-YTD          PIC S9(13)V99 COMP-3.
           05  HESTAT-EFFECTIVE-DATE       PIC 9(08).
           05  HESTAT-OPEN-DATE            PIC 9(08).
           05  HESTAT-LAST-MAINT-DATE      PIC 9(08).
           05  HESTAT-CLOSE-DATE           PIC 9(08).
           05  HESTAT-REASON-CD            PIC X(03).
               88  HESTAT-REASON-CD-KST            VALUE 'KST'.
               88  HESTAT-REASON-CD-NWS            VALUE 'NWS'.
               88  HESTAT-REASON-CD-ZJM            VALUE 'ZJM'.
           05  HESTAT-TYPE-CD              PIC X(02).
               88  HESTAT-TYPE-CD-CD               VALUE 'CD'.
               88  HESTAT-TYPE-CD-JW               VALUE 'JW'.
               88  HESTAT-TYPE-CD-KV               VALUE 'KV'.
               88  HESTAT-TYPE-CD-WS               VALUE 'WS'.
               88  HESTAT-TYPE-CD-XL               VALUE 'XL'.
               88  HESTAT-TYPE-CD-ZV               VALUE 'ZV'.
           05  HESTAT-REGION-CD            PIC X(03).
               88  HESTAT-REGION-CD-BSD            VALUE 'BSD'.
               88  HESTAT-REGION-CD-HKW            VALUE 'HKW'.
               88  HESTAT-REGION-CD-HPB            VALUE 'HPB'.
               88  HESTAT-REGION-CD-ULJ            VALUE 'ULJ'.
           05  HESTAT-DRAW-PERIOD-CD       PIC X(03).
               88  HESTAT-DRAW-PERIOD-CD-CCV       VALUE 'CCV'.
               88  HESTAT-DRAW-PERIOD-CD-JSU       VALUE 'JSU'.
               88  HESTAT-DRAW-PERIOD-CD-KXA       VALUE 'KXA'.
               88  HESTAT-DRAW-PERIOD-CD-VMR       VALUE 'VMR'.
               88  HESTAT-DRAW-PERIOD-CD-WLJ       VALUE 'WLJ'.
               88  HESTAT-DRAW-PERIOD-CD-XRA       VALUE 'XRA'.
           05  HESTAT-PREFERRED-FLAG       PIC X(01).
               88  HESTAT-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  HESTAT-AUTO-RENEW-FLAG      PIC X(01).
               88  HESTAT-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  HESTAT-OVERRIDE-FLAG        PIC X(01).
               88  HESTAT-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  HESTAT-RETRY-CNT            PIC 9(07).
           05  HESTAT-PAYMENT-CNT          PIC 9(07).
           05  HESTAT-DISCOUNT             PIC 9(03)V9(06).
           05  HESTAT-MARGIN               PIC 9(03)V9(06).
           05  HESTAT-CONTACT              PIC X(20).
           05  HESTAT-CUST-ID              PIC X(10).
           05  HESTAT-RETURN-CODE          PIC 9(02).
