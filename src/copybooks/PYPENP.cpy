      *================================================================*
      * COPYBOOK : PYPENP                                              *
      * PURPOSE  : LINKAGE FOR PAYROLL SERVICES RULE MODULE PYPEN      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-09-28 BHS  ORIGINAL                                       *
      * 1992-03-12 BHS  NET EXCEPTIONS TO OPS QUEUE PRB8303            *
      * 1995-08-12 EWS  FED-TAX CALCULATION CORRECTED PRB0226          *
      * 1998-10-12 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2007-10-28 EWS  STATE-TAX NOTICE TEXT PER LEGAL INC1968        *
      * 2010-03-22 PQR  FED-TAX STATUS CODES ADDED PRB3410             *
      * 2012-01-20 TWH  FED-TAX CALCULATION CORRECTED REQ9483          *
      *================================================================*
       01  PYPEN-PARMS.
           05  PYPEN-NET-AMT               PIC S9(13)V99 COMP-3.
           05  PYPEN-FED-TAX-YTD           PIC S9(13)V99 COMP-3.
           05  PYPEN-GROSS-AMT             PIC S9(13)V99 COMP-3.
           05  PYPEN-DEDUCT-YTD            PIC S9(13)V99 COMP-3.
           05  PYPEN-FICA-YTD              PIC S9(13)V99 COMP-3.
           05  PYPEN-STATE-TAX-YTD         PIC S9(13)V99 COMP-3.
           05  PYPEN-EFFECTIVE-DATE        PIC 9(08).
           05  PYPEN-CLOSE-DATE            PIC 9(08).
           05  PYPEN-NEXT-DUE-DATE         PIC 9(08).
           05  PYPEN-EXPIRY-DATE           PIC 9(08).
           05  PYPEN-SOURCE-CD             PIC X(02).
               88  PYPEN-SOURCE-CD-NY              VALUE 'NY'.
               88  PYPEN-SOURCE-CD-PL              VALUE 'PL'.
               88  PYPEN-SOURCE-CD-VK              VALUE 'VK'.
               88  PYPEN-SOURCE-CD-WB              VALUE 'WB'.
               88  PYPEN-SOURCE-CD-XL              VALUE 'XL'.
           05  PYPEN-REASON-CD             PIC X(01).
           05  PYPEN-PAY-FREQ-CD           PIC X(02).
               88  PYPEN-PAY-FREQ-CD-JT            VALUE 'JT'.
               88  PYPEN-PAY-FREQ-CD-XM            VALUE 'XM'.
               88  PYPEN-PAY-FREQ-CD-ZT            VALUE 'ZT'.
           05  PYPEN-TAX-STATE-CD          PIC X(03).
               88  PYPEN-TAX-STATE-CD-CAZ          VALUE 'CAZ'.
               88  PYPEN-TAX-STATE-CD-LZU          VALUE 'LZU'.
               88  PYPEN-TAX-STATE-CD-TUF          VALUE 'TUF'.
               88  PYPEN-TAX-STATE-CD-WKE          VALUE 'WKE'.
           05  PYPEN-PREFERRED-FLAG        PIC X(01).
               88  PYPEN-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  PYPEN-SUSPEND-FLAG          PIC X(01).
               88  PYPEN-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  PYPEN-STOP-FLAG             PIC X(01).
               88  PYPEN-STOP-FLAG-YES             VALUE 'Y'.
           05  PYPEN-PAYMENT-CNT           PIC 9(07).
           05  PYPEN-CYCLE-CNT             PIC 9(07).
           05  PYPEN-MARGIN                PIC 9(03)V9(06).
           05  PYPEN-RATE                  PIC 9(03)V9(06).
           05  PYPEN-CUST-ID               PIC X(10).
           05  PYPEN-REF-NO                PIC X(16).
           05  PYPEN-RETURN-CODE           PIC 9(02).
