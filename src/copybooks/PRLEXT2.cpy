      *================================================================*
      * COPYBOOK : PRLEXT2                                             *
      * PURPOSE  : PAYROLL SERVICES HISTORY RECORD LAYOUT              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-11-27 CRP  ORIGINAL                                       *
      * 1996-09-17 LMB  GROSS LIMIT REVISED CHG4133                    *
      * 2002-09-19 DPS  DEDUCT EXCEPTIONS TO OPS QUEUE PRB9647         *
      * 2009-06-27 EWS  STATE-TAX RERUN PROTECTION INC6336             *
      * 2009-08-10 CRP  PERFORMANCE - GROSS READ AHEAD PRB6912         *
      * 2011-06-21 GAF  FED-TAX LIMIT REVISED CHG5700                  *
      * 2012-05-17 PQR  PERFORMANCE - GROSS READ AHEAD PRB5244         *
      *================================================================*
       01  PRL-H2-REC.
           05  PRLH2-ADDR                  PIC X(30).
           05  PRLH2-KEY                   PIC X(12).
           05  PRLH2-CONTACT               PIC X(20).
           05  PRLH2-REF-NO                PIC X(16).
           05  PRLH2-DESC                  PIC X(30).
           05  PRLH2-NAME                  PIC X(30).
           05  PRLH2-CUST-ID               PIC X(10).
           05  PRL-H2-AMOUNTS.
               10  PRLH2-DEDUCT-AMT        PIC S9(13)V99 COMP-3.
               10  PRLH2-FICA-YTD          PIC S9(13)V99 COMP-3.
               10  PRLH2-NET-AMT           PIC S9(13)V99 COMP-3.
               10  PRLH2-STATE-TAX-AMT     PIC S9(13)V99 COMP-3.
               10  PRLH2-FED-TAX-YTD       PIC S9(13)V99 COMP-3.
               10  PRLH2-GROSS-YTD         PIC S9(13)V99 COMP-3.
               10  PRLH2-DEDUCT-YTD        PIC S9(13)V99 COMP-3.
               10  PRLH2-GROSS-AMT         PIC S9(13)V99 COMP-3.
               10  PRLH2-STATE-TAX-YTD     PIC S9(13)V99 COMP-3.
           05  PRL-H2-DATES.
               10  PRLH2-EXPIRY-DATE       PIC 9(08).
               10  PRLH2-NOTICE-DATE       PIC 9(08).
               10  PRLH2-NEXT-DUE-DATE     PIC 9(08).
               10  PRLH2-LAST-ACT-DATE     PIC 9(08).
               10  PRLH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  PRLH2-OPEN-DATE         PIC 9(08).
               10  PRLH2-LAST-PAY-DATE     PIC 9(08).
               10  PRLH2-EFFECTIVE-DATE    PIC 9(08).
           05  PRL-H2-CODES.
               10  PRLH2-PAY-FREQ-CD       PIC X(02).
                   88  PRLH2-PAY-FREQ-CD-JT        VALUE 'JT'.
                   88  PRLH2-PAY-FREQ-CD-XM        VALUE 'XM'.
                   88  PRLH2-PAY-FREQ-CD-ZT        VALUE 'ZT'.
               10  PRLH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  PRLH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  PRLH2-HOLD-FLAG         PIC X(01).
                   88  PRLH2-HOLD-FLAG-YES         VALUE 'Y'.
               10  PRLH2-PREFERRED-FLAG    PIC X(01).
                   88  PRLH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  PRLH2-SUSPEND-FLAG      PIC X(01).
                   88  PRLH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  PRLH2-SOURCE-CD         PIC X(02).
                   88  PRLH2-SOURCE-CD-NY          VALUE 'NY'.
                   88  PRLH2-SOURCE-CD-PL          VALUE 'PL'.
                   88  PRLH2-SOURCE-CD-VK          VALUE 'VK'.
                   88  PRLH2-SOURCE-CD-WB          VALUE 'WB'.
                   88  PRLH2-SOURCE-CD-XL          VALUE 'XL'.
               10  PRLH2-STATUS-CD         PIC X(02).
                   88  PRLH2-STATUS-CD-MV          VALUE 'MV'.
                   88  PRLH2-STATUS-CD-RD          VALUE 'RD'.
                   88  PRLH2-STATUS-CD-ST          VALUE 'ST'.
                   88  PRLH2-STATUS-CD-TJ          VALUE 'TJ'.
                   88  PRLH2-STATUS-CD-UC          VALUE 'UC'.
               10  PRLH2-STOP-FLAG         PIC X(01).
                   88  PRLH2-STOP-FLAG-YES         VALUE 'Y'.
               10  PRLH2-TAX-STATE-CD      PIC X(03).
                   88  PRLH2-TAX-STATE-CD-CAZ      VALUE 'CAZ'.
                   88  PRLH2-TAX-STATE-CD-LZU      VALUE 'LZU'.
                   88  PRLH2-TAX-STATE-CD-TUF      VALUE 'TUF'.
                   88  PRLH2-TAX-STATE-CD-WKE      VALUE 'WKE'.
           05  PRL-H2-COUNTERS.
               10  PRLH2-NOTICE-CNT        PIC 9(07).
               10  PRLH2-MARGIN            PIC 9(03)V9(06).
               10  PRLH2-CYCLE-CNT         PIC 9(07).
               10  PRLH2-PAYMENT-CNT       PIC 9(07).
               10  PRLH2-FEE-PCT           PIC 9(03)V9(06).
               10  PRLH2-RATE              PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
