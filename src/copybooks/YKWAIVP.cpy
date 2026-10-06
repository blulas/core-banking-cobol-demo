      *================================================================*
      * COPYBOOK : YKWAIVP                                             *
      * PURPOSE  : LINKAGE FOR DATE WINDOW BRIDGE RULE MODULE YKWAIV   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-27 RJT  ORIGINAL                                       *
      * 1994-10-16 GAF  CONTROL NOTICE TEXT PER LEGAL CHG1569          *
      * 1996-12-13 DPS  AUDIT FINDING - AMOUNT-1 CONTROL TOTALS CHG9379*
      * 1996-07-18 PQR  AMOUNT-2 EXCEPTIONS TO OPS QUEUE PRB8554       *
      * 2000-02-19 JLM  AMOUNT-2 STATUS CODES ADDED REQ9724            *
      * 2002-01-06 MKL  CONTROL TABLE EXPANDED REQ8622                 *
      * 2003-06-05 TWH  CONTROL CALCULATION CORRECTED REQ8512          *
      * 2004-08-16 MKL  NEW AMOUNT-1 REPORT COLUMNS INC6951            *
      * 2006-05-11 LMB  AMOUNT-1 RERUN PROTECTION INC3414              *
      * 2006-03-20 TWH  CONTROL CALCULATION CORRECTED PRB9076          *
      * 2008-06-09 EWS  AUDIT FINDING - CONTROL CONTROL TOTALS CHG7980 *
      *================================================================*
       01  YKWAIV-PARMS.
           05  YKWAIV-AMOUNT-1-YTD         PIC S9(13)V99 COMP-3.
           05  YKWAIV-CONTROL-AMT          PIC S9(13)V99 COMP-3.
           05  YKWAIV-AMOUNT-1-AMT         PIC S9(13)V99 COMP-3.
           05  YKWAIV-AMOUNT-2-YTD         PIC S9(13)V99 COMP-3.
           05  YKWAIV-CONTROL-YTD          PIC S9(13)V99 COMP-3.
           05  YKWAIV-AMOUNT-2-AMT         PIC S9(13)V99 COMP-3.
           05  YKWAIV-NOTICE-DATE          PIC 9(08).
           05  YKWAIV-LAST-ACT-DATE        PIC 9(08).
           05  YKWAIV-MATURITY-DATE        PIC 9(08).
           05  YKWAIV-NEXT-DUE-DATE        PIC 9(08).
           05  YKWAIV-SOURCE-CD            PIC X(03).
               88  YKWAIV-SOURCE-CD-BNX            VALUE 'BNX'.
               88  YKWAIV-SOURCE-CD-FTB            VALUE 'FTB'.
               88  YKWAIV-SOURCE-CD-GAP            VALUE 'GAP'.
               88  YKWAIV-SOURCE-CD-PAP            VALUE 'PAP'.
               88  YKWAIV-SOURCE-CD-VGC            VALUE 'VGC'.
               88  YKWAIV-SOURCE-CD-VJV            VALUE 'VJV'.
           05  YKWAIV-CYCLE-CD             PIC X(02).
               88  YKWAIV-CYCLE-CD-FJ              VALUE 'FJ'.
               88  YKWAIV-CYCLE-CD-LA              VALUE 'LA'.
               88  YKWAIV-CYCLE-CD-SP              VALUE 'SP'.
               88  YKWAIV-CYCLE-CD-UC              VALUE 'UC'.
           05  YKWAIV-REASON-CD            PIC X(02).
               88  YKWAIV-REASON-CD-AX             VALUE 'AX'.
               88  YKWAIV-REASON-CD-LK             VALUE 'LK'.
               88  YKWAIV-REASON-CD-NT             VALUE 'NT'.
               88  YKWAIV-REASON-CD-UW             VALUE 'UW'.
           05  YKWAIV-REGION-CD            PIC X(01).
           05  YKWAIV-AUTO-RENEW-FLAG      PIC X(01).
               88  YKWAIV-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  YKWAIV-NOTICE-SENT-FLAG     PIC X(01).
               88  YKWAIV-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  YKWAIV-PREFERRED-FLAG       PIC X(01).
               88  YKWAIV-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  YKWAIV-PAYMENT-CNT          PIC 9(07).
           05  YKWAIV-CYCLE-CNT            PIC 9(07).
           05  YKWAIV-DISCOUNT             PIC 9(03)V9(06).
           05  YKWAIV-MARGIN               PIC 9(03)V9(06).
           05  YKWAIV-CUST-ID              PIC X(10).
           05  YKWAIV-CONTACT              PIC X(20).
           05  YKWAIV-RETURN-CODE          PIC 9(02).
