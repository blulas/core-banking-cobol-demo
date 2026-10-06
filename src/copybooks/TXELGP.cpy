      *================================================================*
      * COPYBOOK : TXELGP                                              *
      * PURPOSE  : LINKAGE FOR TELEX WIRE MESSAGING RULE MODULE TXELG  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-05-23 PQR  ORIGINAL                                       *
      * 1995-10-20 MKL  CORRESP FEE SCHEDULE UPDATE PRB2481            *
      * 1997-08-10 DPS  CHARGE STATUS CODES ADDED PRB5682              *
      * 1998-04-17 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-08-13 JLM  NEW COVER REPORT COLUMNS REQ1929               *
      * 2001-12-28 RJT  QUEUED STATUS CODES ADDED REQ4506              *
      * 2005-10-25 RJT  CHARGE EXCEPTIONS TO OPS QUEUE REQ4631         *
      * 2007-05-03 GAF  NEW WIRE REPORT COLUMNS INC9205                *
      * 2007-01-06 DPS  CORRESP NOTICE TEXT PER LEGAL INC8785          *
      * 2008-10-05 JLM  NEW COVER REPORT COLUMNS REQ2415               *
      *================================================================*
       01  TXELG-PARMS.
           05  TXELG-WIRE-YTD              PIC S9(13)V99 COMP-3.
           05  TXELG-CHARGE-AMT            PIC S9(13)V99 COMP-3.
           05  TXELG-QUEUED-AMT            PIC S9(13)V99 COMP-3.
           05  TXELG-CORRESP-YTD           PIC S9(13)V99 COMP-3.
           05  TXELG-CORRESP-AMT           PIC S9(13)V99 COMP-3.
           05  TXELG-WIRE-AMT              PIC S9(13)V99 COMP-3.
           05  TXELG-LAST-PAY-DATE         PIC 9(08).
           05  TXELG-NEXT-DUE-DATE         PIC 9(08).
           05  TXELG-EFFECTIVE-DATE        PIC 9(08).
           05  TXELG-CLOSE-DATE            PIC 9(08).
           05  TXELG-MSG-TYPE-CD           PIC X(02).
               88  TXELG-MSG-TYPE-CD-LG            VALUE 'LG'.
               88  TXELG-MSG-TYPE-CD-LX            VALUE 'LX'.
               88  TXELG-MSG-TYPE-CD-ND            VALUE 'ND'.
               88  TXELG-MSG-TYPE-CD-PT            VALUE 'PT'.
           05  TXELG-REGION-CD             PIC X(01).
               88  TXELG-REGION-CD-G               VALUE 'G'.
               88  TXELG-REGION-CD-M               VALUE 'M'.
               88  TXELG-REGION-CD-T               VALUE 'T'.
           05  TXELG-TYPE-CD               PIC X(01).
               88  TXELG-TYPE-CD-K                 VALUE 'K'.
               88  TXELG-TYPE-CD-L                 VALUE 'L'.
               88  TXELG-TYPE-CD-M                 VALUE 'M'.
               88  TXELG-TYPE-CD-P                 VALUE 'P'.
               88  TXELG-TYPE-CD-W                 VALUE 'W'.
               88  TXELG-TYPE-CD-Z                 VALUE 'Z'.
           05  TXELG-REASON-CD             PIC X(03).
           05  TXELG-SUSPEND-FLAG          PIC X(01).
               88  TXELG-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  TXELG-STOP-FLAG             PIC X(01).
               88  TXELG-STOP-FLAG-YES             VALUE 'Y'.
           05  TXELG-OVERRIDE-FLAG         PIC X(01).
               88  TXELG-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  TXELG-PAYMENT-CNT           PIC 9(07).
           05  TXELG-NOTICE-CNT            PIC 9(07).
           05  TXELG-FEE-PCT               PIC 9(03)V9(06).
           05  TXELG-RATE                  PIC 9(03)V9(06).
           05  TXELG-ADDR                  PIC X(30).
           05  TXELG-DESC                  PIC X(30).
           05  TXELG-RETURN-CODE           PIC 9(02).
