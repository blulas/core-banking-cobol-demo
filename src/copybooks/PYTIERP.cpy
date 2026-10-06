      *================================================================*
      * COPYBOOK : PYTIERP                                             *
      * PURPOSE  : LINKAGE FOR PAYROLL SERVICES RULE MODULE PYTIER     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-10-13 RJT  ORIGINAL                                       *
      * 1996-09-23 EWS  NEW FED-TAX REPORT COLUMNS REQ2383             *
      * 2000-10-09 LMB  NEW STATE-TAX REPORT COLUMNS PRB9566           *
      * 2003-03-05 TWH  PERFORMANCE - FICA READ AHEAD PRB8934          *
      * 2004-01-14 PQR  NEW FICA REPORT COLUMNS PRB8727                *
      * 2004-08-28 RJT  STATE-TAX TABLE EXPANDED PRB8241               *
      * 2007-06-20 DPS  GROSS ROUNDING - HALF UP PRB8545               *
      * 2007-08-08 MKL  STATE-TAX CALCULATION CORRECTED INC4702        *
      * 2008-04-08 LMB  NET CUTOFF MOVED REQ3600                       *
      * 2009-04-06 GAF  DEDUCT ROUNDING - HALF UP CHG1777              *
      * 2009-06-21 DPS  PERFORMANCE - FICA READ AHEAD PRB9784          *
      * 2012-09-23 EWS  GROSS CUTOFF MOVED REQ9742                     *
      *================================================================*
       01  PYTIER-PARMS.
           05  PYTIER-DEDUCT-AMT           PIC S9(13)V99 COMP-3.
           05  PYTIER-NET-YTD              PIC S9(13)V99 COMP-3.
           05  PYTIER-STATE-TAX-AMT        PIC S9(13)V99 COMP-3.
           05  PYTIER-NET-AMT              PIC S9(13)V99 COMP-3.
           05  PYTIER-GROSS-AMT            PIC S9(13)V99 COMP-3.
           05  PYTIER-FED-TAX-AMT          PIC S9(13)V99 COMP-3.
           05  PYTIER-NEXT-DUE-DATE        PIC 9(08).
           05  PYTIER-EXPIRY-DATE          PIC 9(08).
           05  PYTIER-CLOSE-DATE           PIC 9(08).
           05  PYTIER-OPEN-DATE            PIC 9(08).
           05  PYTIER-PAY-FREQ-CD          PIC X(02).
               88  PYTIER-PAY-FREQ-CD-JT           VALUE 'JT'.
               88  PYTIER-PAY-FREQ-CD-XM           VALUE 'XM'.
               88  PYTIER-PAY-FREQ-CD-ZT           VALUE 'ZT'.
           05  PYTIER-TAX-STATE-CD         PIC X(03).
               88  PYTIER-TAX-STATE-CD-CAZ         VALUE 'CAZ'.
               88  PYTIER-TAX-STATE-CD-LZU         VALUE 'LZU'.
               88  PYTIER-TAX-STATE-CD-TUF         VALUE 'TUF'.
               88  PYTIER-TAX-STATE-CD-WKE         VALUE 'WKE'.
           05  PYTIER-STATUS-CD            PIC X(02).
               88  PYTIER-STATUS-CD-MV             VALUE 'MV'.
               88  PYTIER-STATUS-CD-RD             VALUE 'RD'.
               88  PYTIER-STATUS-CD-ST             VALUE 'ST'.
               88  PYTIER-STATUS-CD-TJ             VALUE 'TJ'.
               88  PYTIER-STATUS-CD-UC             VALUE 'UC'.
           05  PYTIER-REASON-CD            PIC X(01).
           05  PYTIER-SUSPEND-FLAG         PIC X(01).
               88  PYTIER-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  PYTIER-HOLD-FLAG            PIC X(01).
               88  PYTIER-HOLD-FLAG-YES            VALUE 'Y'.
           05  PYTIER-PREFERRED-FLAG       PIC X(01).
               88  PYTIER-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  PYTIER-ITEM-CNT             PIC 9(07).
           05  PYTIER-PAYMENT-CNT          PIC 9(07).
           05  PYTIER-RATE                 PIC 9(03)V9(06).
           05  PYTIER-FEE-PCT              PIC 9(03)V9(06).
           05  PYTIER-DESC                 PIC X(30).
           05  PYTIER-CONTACT              PIC X(20).
           05  PYTIER-RETURN-CODE          PIC 9(02).
