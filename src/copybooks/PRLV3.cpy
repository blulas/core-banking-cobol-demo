      *================================================================*
      * COPYBOOK : PRLV3                                               *
      * PURPOSE  : PAYROLL SERVICES ARCHIVE RECORD LAYOUT              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-05-26 PQR  ORIGINAL                                       *
      * 1996-11-07 EWS  FED-TAX RERUN PROTECTION PRB0495               *
      * 1997-08-04 RJT  FED-TAX NOTICE TEXT PER LEGAL INC9082          *
      * 1999-08-24 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-09-06 CRP  NET TABLE EXPANDED REQ9765                     *
      * 1999-03-19 TWH  FED-TAX CUTOFF MOVED PRB6372                   *
      * 2000-03-07 LMB  DEDUCT CUTOFF MOVED PRB3073                    *
      * 2000-02-02 LMB  PERFORMANCE - STATE-TAX READ AHEAD INC9911     *
      * 2005-10-21 SKN  NET LIMIT REVISED INC8434                      *
      * 2006-07-19 BHS  NET STATUS CODES ADDED CHG1868                 *
      * 2010-12-27 TWH  DEDUCT FEE SCHEDULE UPDATE CHG9170             *
      * 2010-01-05 DPS  FED-TAX TABLE EXPANDED INC8921                 *
      *================================================================*
       01  PRL-H3-REC.
           05  PRLH3-KEY                   PIC X(12).
           05  PRLH3-CUST-ID               PIC X(10).
           05  PRLH3-DESC                  PIC X(30).
           05  PRLH3-REF-NO                PIC X(16).
           05  PRLH3-CONTACT               PIC X(20).
           05  PRLH3-NAME                  PIC X(30).
           05  PRLH3-ADDR                  PIC X(30).
           05  PRL-H3-AMOUNTS.
               10  PRLH3-FICA-YTD          PIC S9(13)V99 COMP-3.
               10  PRLH3-GROSS-YTD         PIC S9(13)V99 COMP-3.
               10  PRLH3-GROSS-AMT         PIC S9(13)V99 COMP-3.
               10  PRLH3-NET-AMT           PIC S9(13)V99 COMP-3.
               10  PRLH3-DEDUCT-YTD        PIC S9(13)V99 COMP-3.
               10  PRLH3-DEDUCT-AMT        PIC S9(13)V99 COMP-3.
               10  PRLH3-FICA-AMT          PIC S9(13)V99 COMP-3.
               10  PRLH3-STATE-TAX-YTD     PIC S9(13)V99 COMP-3.
               10  PRLH3-NET-YTD           PIC S9(13)V99 COMP-3.
               10  PRLH3-FED-TAX-YTD       PIC S9(13)V99 COMP-3.
               10  PRLH3-STATE-TAX-AMT     PIC S9(13)V99 COMP-3.
           05  PRL-H3-DATES.
               10  PRLH3-LAST-PAY-DATE     PIC 9(08).
               10  PRLH3-NEXT-DUE-DATE     PIC 9(08).
               10  PRLH3-OPEN-DATE         PIC 9(08).
               10  PRLH3-CLOSE-DATE        PIC 9(08).
               10  PRLH3-LAST-ACT-DATE     PIC 9(08).
               10  PRLH3-EXPIRY-DATE       PIC 9(08).
               10  PRLH3-NOTICE-DATE       PIC 9(08).
               10  PRLH3-NEXT-REVIEW-DATE  PIC 9(08).
           05  PRL-H3-CODES.
               10  PRLH3-STATUS-CD         PIC X(02).
                   88  PRLH3-STATUS-CD-MV          VALUE 'MV'.
                   88  PRLH3-STATUS-CD-RD          VALUE 'RD'.
                   88  PRLH3-STATUS-CD-ST          VALUE 'ST'.
                   88  PRLH3-STATUS-CD-TJ          VALUE 'TJ'.
                   88  PRLH3-STATUS-CD-UC          VALUE 'UC'.
               10  PRLH3-REGION-CD         PIC X(02).
                   88  PRLH3-REGION-CD-DP          VALUE 'DP'.
                   88  PRLH3-REGION-CD-TU          VALUE 'TU'.
                   88  PRLH3-REGION-CD-WD          VALUE 'WD'.
               10  PRLH3-PAY-FREQ-CD       PIC X(02).
                   88  PRLH3-PAY-FREQ-CD-JT        VALUE 'JT'.
                   88  PRLH3-PAY-FREQ-CD-XM        VALUE 'XM'.
                   88  PRLH3-PAY-FREQ-CD-ZT        VALUE 'ZT'.
               10  PRLH3-HOLD-FLAG         PIC X(01).
                   88  PRLH3-HOLD-FLAG-YES         VALUE 'Y'.
               10  PRLH3-REASON-CD         PIC X(01).
               10  PRLH3-AUTO-RENEW-FLAG   PIC X(01).
                   88  PRLH3-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  PRLH3-SUSPEND-FLAG      PIC X(01).
                   88  PRLH3-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  PRLH3-TAX-STATE-CD      PIC X(03).
                   88  PRLH3-TAX-STATE-CD-CAZ      VALUE 'CAZ'.
                   88  PRLH3-TAX-STATE-CD-LZU      VALUE 'LZU'.
                   88  PRLH3-TAX-STATE-CD-TUF      VALUE 'TUF'.
                   88  PRLH3-TAX-STATE-CD-WKE      VALUE 'WKE'.
               10  PRLH3-NOTICE-SENT-FLAG  PIC X(01).
                   88  PRLH3-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  PRLH3-PREFERRED-FLAG    PIC X(01).
                   88  PRLH3-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  PRLH3-STOP-FLAG         PIC X(01).
                   88  PRLH3-STOP-FLAG-YES         VALUE 'Y'.
           05  PRL-H3-COUNTERS.
               10  PRLH3-CYCLE-CNT         PIC 9(07).
               10  PRLH3-RATE              PIC 9(03)V9(06).
               10  PRLH3-PAYMENT-CNT       PIC 9(07).
               10  PRLH3-FEE-PCT           PIC 9(03)V9(06).
               10  PRLH3-MARGIN            PIC 9(03)V9(06).
               10  PRLH3-NOTICE-CNT        PIC 9(07).
           05  FILLER                      PIC X(20).
