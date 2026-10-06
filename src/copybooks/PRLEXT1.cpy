      *================================================================*
      * COPYBOOK : PRLEXT1                                             *
      * PURPOSE  : PAYROLL SERVICES HISTORY RECORD LAYOUT              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-10-27 DPS  ORIGINAL                                       *
      * 1997-06-07 TWH  FED-TAX EXCEPTIONS TO OPS QUEUE REQ0949        *
      * 1997-11-03 MKL  NEW FICA REPORT COLUMNS CHG6666                *
      * 1998-01-11 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-14 BHS  NEW STATE-TAX REPORT COLUMNS CHG0921           *
      * 2001-07-13 JLM  STATE-TAX LIMIT REVISED INC9797                *
      * 2004-03-01 BHS  PERFORMANCE - STATE-TAX READ AHEAD PRB8712     *
      * 2010-08-26 EWS  STATE-TAX LIMIT REVISED REQ2954                *
      * 2012-07-24 CRP  GROSS LIMIT REVISED CHG9282                    *
      * 2012-08-09 GAF  NET EXCEPTIONS TO OPS QUEUE REQ8075            *
      *================================================================*
       01  PRL-H1-REC.
           05  PRLH1-ADDR                  PIC X(30).
           05  PRLH1-KEY                   PIC X(12).
           05  PRLH1-CONTACT               PIC X(20).
           05  PRLH1-CUST-ID               PIC X(10).
           05  PRLH1-DESC                  PIC X(30).
           05  PRLH1-REF-NO                PIC X(16).
           05  PRLH1-NAME                  PIC X(30).
           05  PRL-H1-AMOUNTS.
               10  PRLH1-NET-YTD           PIC S9(13)V99 COMP-3.
               10  PRLH1-GROSS-YTD         PIC S9(13)V99 COMP-3.
               10  PRLH1-GROSS-AMT         PIC S9(13)V99 COMP-3.
               10  PRLH1-DEDUCT-AMT        PIC S9(13)V99 COMP-3.
               10  PRLH1-FED-TAX-YTD       PIC S9(13)V99 COMP-3.
               10  PRLH1-STATE-TAX-AMT     PIC S9(13)V99 COMP-3.
               10  PRLH1-DEDUCT-YTD        PIC S9(13)V99 COMP-3.
               10  PRLH1-STATE-TAX-YTD     PIC S9(13)V99 COMP-3.
               10  PRLH1-FICA-AMT          PIC S9(13)V99 COMP-3.
               10  PRLH1-FICA-YTD          PIC S9(13)V99 COMP-3.
               10  PRLH1-NET-AMT           PIC S9(13)V99 COMP-3.
           05  PRL-H1-DATES.
               10  PRLH1-LAST-ACT-DATE     PIC 9(08).
               10  PRLH1-LAST-PAY-DATE     PIC 9(08).
               10  PRLH1-EFFECTIVE-DATE    PIC 9(08).
               10  PRLH1-CLOSE-DATE        PIC 9(08).
               10  PRLH1-NEXT-DUE-DATE     PIC 9(08).
               10  PRLH1-EXPIRY-DATE       PIC 9(08).
               10  PRLH1-NOTICE-DATE       PIC 9(08).
               10  PRLH1-NEXT-REVIEW-DATE  PIC 9(08).
           05  PRL-H1-CODES.
               10  PRLH1-SOURCE-CD         PIC X(02).
                   88  PRLH1-SOURCE-CD-NY          VALUE 'NY'.
                   88  PRLH1-SOURCE-CD-PL          VALUE 'PL'.
                   88  PRLH1-SOURCE-CD-VK          VALUE 'VK'.
                   88  PRLH1-SOURCE-CD-WB          VALUE 'WB'.
                   88  PRLH1-SOURCE-CD-XL          VALUE 'XL'.
               10  PRLH1-SUSPEND-FLAG      PIC X(01).
                   88  PRLH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  PRLH1-REGION-CD         PIC X(02).
                   88  PRLH1-REGION-CD-DP          VALUE 'DP'.
                   88  PRLH1-REGION-CD-TU          VALUE 'TU'.
                   88  PRLH1-REGION-CD-WD          VALUE 'WD'.
               10  PRLH1-REASON-CD         PIC X(01).
               10  PRLH1-STOP-FLAG         PIC X(01).
                   88  PRLH1-STOP-FLAG-YES         VALUE 'Y'.
               10  PRLH1-PAY-FREQ-CD       PIC X(02).
                   88  PRLH1-PAY-FREQ-CD-JT        VALUE 'JT'.
                   88  PRLH1-PAY-FREQ-CD-XM        VALUE 'XM'.
                   88  PRLH1-PAY-FREQ-CD-ZT        VALUE 'ZT'.
               10  PRLH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  PRLH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  PRLH1-HOLD-FLAG         PIC X(01).
                   88  PRLH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  PRLH1-PREFERRED-FLAG    PIC X(01).
                   88  PRLH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  PRLH1-STATUS-CD         PIC X(02).
                   88  PRLH1-STATUS-CD-MV          VALUE 'MV'.
                   88  PRLH1-STATUS-CD-RD          VALUE 'RD'.
                   88  PRLH1-STATUS-CD-ST          VALUE 'ST'.
                   88  PRLH1-STATUS-CD-TJ          VALUE 'TJ'.
                   88  PRLH1-STATUS-CD-UC          VALUE 'UC'.
           05  PRL-H1-COUNTERS.
               10  PRLH1-NOTICE-CNT        PIC 9(07).
               10  PRLH1-RATE              PIC 9(03)V9(06).
               10  PRLH1-MARGIN            PIC 9(03)V9(06).
               10  PRLH1-FEE-PCT           PIC 9(03)V9(06).
               10  PRLH1-CYCLE-CNT         PIC 9(07).
               10  PRLH1-ITEM-CNT          PIC 9(07).
               10  PRLH1-PAYMENT-CNT       PIC 9(07).
           05  FILLER                      PIC X(20).
