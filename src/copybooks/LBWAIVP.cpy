      *================================================================*
      * COPYBOOK : LBWAIVP                                             *
      * PURPOSE  : LINKAGE FOR LOCKBOX REMITTANCE RULE MODULE LBWAIV   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-06-15 RJT  ORIGINAL                                       *
      * 2004-03-22 BHS  REMIT STATUS CODES ADDED REQ0781               *
      * 2009-09-03 DPS  NEW CASH-ON-HAND REPORT COLUMNS CHG4006        *
      * 2010-07-26 PQR  AUDIT FINDING - CASH-ON-HAND CONTROL TOTALS REQ*
      * 2011-12-08 MKL  FEE CUTOFF MOVED PRB2112                       *
      * 2013-12-20 JLM  REMIT EXCEPTIONS TO OPS QUEUE CHG4119          *
      * 2014-02-19 JLM  EXCEPT ROUNDING - HALF UP PRB9108              *
      *================================================================*
       01  LBWAIV-PARMS.
           05  LBWAIV-EXCEPT-AMT           PIC S9(13)V99 COMP-3.
           05  LBWAIV-FLOAT-AMT            PIC S9(13)V99 COMP-3.
           05  LBWAIV-CHECK-YTD            PIC S9(13)V99 COMP-3.
           05  LBWAIV-CASH-ON-HAND-YTD     PIC S9(13)V99 COMP-3.
           05  LBWAIV-FLOAT-YTD            PIC S9(13)V99 COMP-3.
           05  LBWAIV-REMIT-YTD            PIC S9(13)V99 COMP-3.
           05  LBWAIV-MATURITY-DATE        PIC 9(08).
           05  LBWAIV-EXPIRY-DATE          PIC 9(08).
           05  LBWAIV-LAST-PAY-DATE        PIC 9(08).
           05  LBWAIV-LAST-STMT-DATE       PIC 9(08).
           05  LBWAIV-REASON-CD            PIC X(02).
               88  LBWAIV-REASON-CD-GF             VALUE 'GF'.
               88  LBWAIV-REASON-CD-UK             VALUE 'UK'.
               88  LBWAIV-REASON-CD-VR             VALUE 'VR'.
               88  LBWAIV-REASON-CD-XG             VALUE 'XG'.
           05  LBWAIV-LOCKBOX-CD           PIC X(02).
               88  LBWAIV-LOCKBOX-CD-DX            VALUE 'DX'.
               88  LBWAIV-LOCKBOX-CD-JM            VALUE 'JM'.
               88  LBWAIV-LOCKBOX-CD-WW            VALUE 'WW'.
               88  LBWAIV-LOCKBOX-CD-XE            VALUE 'XE'.
           05  LBWAIV-REGION-CD            PIC X(03).
               88  LBWAIV-REGION-CD-DVP            VALUE 'DVP'.
               88  LBWAIV-REGION-CD-FMZ            VALUE 'FMZ'.
               88  LBWAIV-REGION-CD-KSL            VALUE 'KSL'.
               88  LBWAIV-REGION-CD-NYK            VALUE 'NYK'.
           05  LBWAIV-CHANNEL-CD           PIC X(01).
               88  LBWAIV-CHANNEL-CD-B             VALUE 'B'.
               88  LBWAIV-CHANNEL-CD-H             VALUE 'H'.
               88  LBWAIV-CHANNEL-CD-Z             VALUE 'Z'.
           05  LBWAIV-PREFERRED-FLAG       PIC X(01).
               88  LBWAIV-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  LBWAIV-EXEMPT-FLAG          PIC X(01).
               88  LBWAIV-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  LBWAIV-STOP-FLAG            PIC X(01).
               88  LBWAIV-STOP-FLAG-YES            VALUE 'Y'.
           05  LBWAIV-PAYMENT-CNT          PIC 9(07).
           05  LBWAIV-EXCEPT-CNT           PIC 9(07).
           05  LBWAIV-PENALTY-PCT          PIC 9(03)V9(06).
           05  LBWAIV-DISCOUNT             PIC 9(03)V9(06).
           05  LBWAIV-CONTACT              PIC X(20).
           05  LBWAIV-CUST-ID              PIC X(10).
           05  LBWAIV-RETURN-CODE          PIC 9(02).
