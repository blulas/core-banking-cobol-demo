      *================================================================*
      * COPYBOOK : YKSTATP                                             *
      * PURPOSE  : LINKAGE FOR DATE WINDOW BRIDGE RULE MODULE YKSTAT   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-07 EWS  ORIGINAL                                       *
      * 1998-05-20 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-18 CRP  AMOUNT-1 LIMIT REVISED REQ7449                 *
      * 2003-02-08 BHS  CONTROL EXCEPTIONS TO OPS QUEUE CHG0984        *
      * 2005-10-20 DPS  AMOUNT-2 STATUS CODES ADDED PRB4779            *
      * 2006-11-13 SKN  AMOUNT-1 EXCEPTIONS TO OPS QUEUE INC5241       *
      * 2008-07-20 CRP  CONTROL NOTICE TEXT PER LEGAL REQ5353          *
      *================================================================*
       01  YKSTAT-PARMS.
           05  YKSTAT-AMOUNT-2-YTD         PIC S9(13)V99 COMP-3.
           05  YKSTAT-AMOUNT-2-AMT         PIC S9(13)V99 COMP-3.
           05  YKSTAT-AMOUNT-1-AMT         PIC S9(13)V99 COMP-3.
           05  YKSTAT-AMOUNT-1-YTD         PIC S9(13)V99 COMP-3.
           05  YKSTAT-CONTROL-YTD          PIC S9(13)V99 COMP-3.
           05  YKSTAT-CONTROL-AMT          PIC S9(13)V99 COMP-3.
           05  YKSTAT-NEXT-DUE-DATE        PIC 9(08).
           05  YKSTAT-LAST-PAY-DATE        PIC 9(08).
           05  YKSTAT-LAST-STMT-DATE       PIC 9(08).
           05  YKSTAT-LAST-MAINT-DATE      PIC 9(08).
           05  YKSTAT-REGION-CD            PIC X(01).
           05  YKSTAT-CYCLE-CD             PIC X(02).
               88  YKSTAT-CYCLE-CD-FJ              VALUE 'FJ'.
               88  YKSTAT-CYCLE-CD-LA              VALUE 'LA'.
               88  YKSTAT-CYCLE-CD-SP              VALUE 'SP'.
               88  YKSTAT-CYCLE-CD-UC              VALUE 'UC'.
           05  YKSTAT-SOURCE-CD            PIC X(03).
               88  YKSTAT-SOURCE-CD-BNX            VALUE 'BNX'.
               88  YKSTAT-SOURCE-CD-FTB            VALUE 'FTB'.
               88  YKSTAT-SOURCE-CD-GAP            VALUE 'GAP'.
               88  YKSTAT-SOURCE-CD-PAP            VALUE 'PAP'.
               88  YKSTAT-SOURCE-CD-VGC            VALUE 'VGC'.
               88  YKSTAT-SOURCE-CD-VJV            VALUE 'VJV'.
           05  YKSTAT-REASON-CD            PIC X(02).
               88  YKSTAT-REASON-CD-AX             VALUE 'AX'.
               88  YKSTAT-REASON-CD-LK             VALUE 'LK'.
               88  YKSTAT-REASON-CD-NT             VALUE 'NT'.
               88  YKSTAT-REASON-CD-UW             VALUE 'UW'.
           05  YKSTAT-AUTO-RENEW-FLAG      PIC X(01).
               88  YKSTAT-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  YKSTAT-PREFERRED-FLAG       PIC X(01).
               88  YKSTAT-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  YKSTAT-NOTICE-SENT-FLAG     PIC X(01).
               88  YKSTAT-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  YKSTAT-RETRY-CNT            PIC 9(07).
           05  YKSTAT-CYCLE-CNT            PIC 9(07).
           05  YKSTAT-PENALTY-PCT          PIC 9(03)V9(06).
           05  YKSTAT-MARGIN               PIC 9(03)V9(06).
           05  YKSTAT-DESC                 PIC X(30).
           05  YKSTAT-CUST-ID              PIC X(10).
           05  YKSTAT-RETURN-CODE          PIC 9(02).
