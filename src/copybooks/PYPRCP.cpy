      *================================================================*
      * COPYBOOK : PYPRCP                                              *
      * PURPOSE  : LINKAGE FOR PAYROLL SERVICES RULE MODULE PYPRC      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-10-27 TWH  ORIGINAL                                       *
      * 1993-03-05 CRP  STATE-TAX TABLE EXPANDED INC9841               *
      * 1995-04-12 BHS  DEDUCT FEE SCHEDULE UPDATE INC5310             *
      * 1996-02-26 SKN  NEW FICA REPORT COLUMNS CHG3491                *
      * 1998-01-11 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-12-05 GAF  FED-TAX CUTOFF MOVED PRB7880                   *
      * 2003-12-27 GAF  NEW FED-TAX REPORT COLUMNS INC1299             *
      * 2003-12-27 DPS  NEW NET REPORT COLUMNS CHG6107                 *
      * 2007-10-02 EWS  NEW STATE-TAX REPORT COLUMNS INC5339           *
      * 2009-04-19 CRP  NET LIMIT REVISED PRB9046                      *
      *================================================================*
       01  PYPRC-PARMS.
           05  PYPRC-STATE-TAX-YTD         PIC S9(13)V99 COMP-3.
           05  PYPRC-FED-TAX-AMT           PIC S9(13)V99 COMP-3.
           05  PYPRC-NET-AMT               PIC S9(13)V99 COMP-3.
           05  PYPRC-STATE-TAX-AMT         PIC S9(13)V99 COMP-3.
           05  PYPRC-FICA-YTD              PIC S9(13)V99 COMP-3.
           05  PYPRC-FED-TAX-YTD           PIC S9(13)V99 COMP-3.
           05  PYPRC-NEXT-DUE-DATE         PIC 9(08).
           05  PYPRC-LAST-PAY-DATE         PIC 9(08).
           05  PYPRC-OPEN-DATE             PIC 9(08).
           05  PYPRC-EXPIRY-DATE           PIC 9(08).
           05  PYPRC-TAX-STATE-CD          PIC X(03).
               88  PYPRC-TAX-STATE-CD-CAZ          VALUE 'CAZ'.
               88  PYPRC-TAX-STATE-CD-LZU          VALUE 'LZU'.
               88  PYPRC-TAX-STATE-CD-TUF          VALUE 'TUF'.
               88  PYPRC-TAX-STATE-CD-WKE          VALUE 'WKE'.
           05  PYPRC-STATUS-CD             PIC X(02).
               88  PYPRC-STATUS-CD-MV              VALUE 'MV'.
               88  PYPRC-STATUS-CD-RD              VALUE 'RD'.
               88  PYPRC-STATUS-CD-ST              VALUE 'ST'.
               88  PYPRC-STATUS-CD-TJ              VALUE 'TJ'.
               88  PYPRC-STATUS-CD-UC              VALUE 'UC'.
           05  PYPRC-SOURCE-CD             PIC X(02).
               88  PYPRC-SOURCE-CD-NY              VALUE 'NY'.
               88  PYPRC-SOURCE-CD-PL              VALUE 'PL'.
               88  PYPRC-SOURCE-CD-VK              VALUE 'VK'.
               88  PYPRC-SOURCE-CD-WB              VALUE 'WB'.
               88  PYPRC-SOURCE-CD-XL              VALUE 'XL'.
           05  PYPRC-REGION-CD             PIC X(02).
               88  PYPRC-REGION-CD-DP              VALUE 'DP'.
               88  PYPRC-REGION-CD-TU              VALUE 'TU'.
               88  PYPRC-REGION-CD-WD              VALUE 'WD'.
           05  PYPRC-STOP-FLAG             PIC X(01).
               88  PYPRC-STOP-FLAG-YES             VALUE 'Y'.
           05  PYPRC-AUTO-RENEW-FLAG       PIC X(01).
               88  PYPRC-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  PYPRC-PREFERRED-FLAG        PIC X(01).
               88  PYPRC-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  PYPRC-NOTICE-CNT            PIC 9(07).
           05  PYPRC-ITEM-CNT              PIC 9(07).
           05  PYPRC-FEE-PCT               PIC 9(03)V9(06).
           05  PYPRC-MARGIN                PIC 9(03)V9(06).
           05  PYPRC-CUST-ID               PIC X(10).
           05  PYPRC-CONTACT               PIC X(20).
           05  PYPRC-RETURN-CODE           PIC 9(02).
