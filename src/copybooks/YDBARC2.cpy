      *================================================================*
      * COPYBOOK : YDBARC2                                             *
      * PURPOSE  : DATE WINDOW BRIDGE ARCHIVE RECORD LAYOUT            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-06 DPS  ORIGINAL                                       *
      * 1994-11-11 DPS  CONTROL NOTICE TEXT PER LEGAL INC8670          *
      * 1997-04-03 DPS  AMOUNT-2 CUTOFF MOVED REQ7292                  *
      * 2002-07-14 BHS  NEW AMOUNT-1 REPORT COLUMNS PRB4047            *
      * 2007-07-21 TWH  AMOUNT-2 NOTICE TEXT PER LEGAL REQ3600         *
      *================================================================*
       01  YDB-H2-REC.
           05  YDBH2-CUST-ID               PIC X(10).
           05  YDBH2-CONTACT               PIC X(20).
           05  YDBH2-REF-NO                PIC X(16).
           05  YDBH2-NAME                  PIC X(30).
           05  YDBH2-KEY                   PIC X(12).
           05  YDBH2-DESC                  PIC X(30).
           05  YDBH2-ADDR                  PIC X(30).
           05  YDB-H2-AMOUNTS.
               10  YDBH2-AMOUNT-1-YTD      PIC S9(13)V99 COMP-3.
               10  YDBH2-AMOUNT-1-AMT      PIC S9(13)V99 COMP-3.
               10  YDBH2-AMOUNT-2-AMT      PIC S9(13)V99 COMP-3.
               10  YDBH2-AMOUNT-2-YTD      PIC S9(13)V99 COMP-3.
               10  YDBH2-CONTROL-AMT       PIC S9(13)V99 COMP-3.
               10  YDBH2-CONTROL-YTD       PIC S9(13)V99 COMP-3.
           05  YDB-H2-DATES.
               10  YDBH2-OPEN-DATE         PIC 9(08).
               10  YDBH2-LAST-PAY-DATE     PIC 9(08).
               10  YDBH2-LAST-STMT-DATE    PIC 9(08).
               10  YDBH2-LAST-ACT-DATE     PIC 9(08).
               10  YDBH2-LAST-MAINT-DATE   PIC 9(08).
               10  YDBH2-MATURITY-DATE     PIC 9(08).
               10  YDBH2-NEXT-DUE-DATE     PIC 9(08).
           05  YDB-H2-CODES.
               10  YDBH2-CYCLE-CD          PIC X(02).
                   88  YDBH2-CYCLE-CD-FJ           VALUE 'FJ'.
                   88  YDBH2-CYCLE-CD-LA           VALUE 'LA'.
                   88  YDBH2-CYCLE-CD-SP           VALUE 'SP'.
                   88  YDBH2-CYCLE-CD-UC           VALUE 'UC'.
               10  YDBH2-SOURCE-CD         PIC X(03).
                   88  YDBH2-SOURCE-CD-BNX         VALUE 'BNX'.
                   88  YDBH2-SOURCE-CD-FTB         VALUE 'FTB'.
                   88  YDBH2-SOURCE-CD-GAP         VALUE 'GAP'.
                   88  YDBH2-SOURCE-CD-PAP         VALUE 'PAP'.
                   88  YDBH2-SOURCE-CD-VGC         VALUE 'VGC'.
                   88  YDBH2-SOURCE-CD-VJV         VALUE 'VJV'.
               10  YDBH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  YDBH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  YDBH2-WINDOW-RULE-CD    PIC X(01).
                   88  YDBH2-WINDOW-RULE-CD-A      VALUE 'A'.
                   88  YDBH2-WINDOW-RULE-CD-D      VALUE 'D'.
                   88  YDBH2-WINDOW-RULE-CD-S      VALUE 'S'.
                   88  YDBH2-WINDOW-RULE-CD-T      VALUE 'T'.
               10  YDBH2-REVIEW-FLAG       PIC X(01).
                   88  YDBH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  YDBH2-AUTO-RENEW-FLAG   PIC X(01).
                   88  YDBH2-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  YDBH2-OVERRIDE-FLAG     PIC X(01).
                   88  YDBH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  YDBH2-PIVOT-CD          PIC X(03).
                   88  YDBH2-PIVOT-CD-KGT          VALUE 'KGT'.
                   88  YDBH2-PIVOT-CD-NNG          VALUE 'NNG'.
                   88  YDBH2-PIVOT-CD-PTC          VALUE 'PTC'.
                   88  YDBH2-PIVOT-CD-WZW          VALUE 'WZW'.
               10  YDBH2-EXEMPT-FLAG       PIC X(01).
                   88  YDBH2-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  YDBH2-REASON-CD         PIC X(02).
                   88  YDBH2-REASON-CD-AX          VALUE 'AX'.
                   88  YDBH2-REASON-CD-LK          VALUE 'LK'.
                   88  YDBH2-REASON-CD-NT          VALUE 'NT'.
                   88  YDBH2-REASON-CD-UW          VALUE 'UW'.
               10  YDBH2-PREFERRED-FLAG    PIC X(01).
                   88  YDBH2-PREFERRED-FLAG-YES    VALUE 'Y'.
           05  YDB-H2-COUNTERS.
               10  YDBH2-CYCLE-CNT         PIC 9(07).
               10  YDBH2-ITEM-CNT          PIC 9(07).
               10  YDBH2-RETRY-CNT         PIC 9(07).
               10  YDBH2-PENALTY-PCT       PIC 9(03)V9(06).
               10  YDBH2-PAYMENT-CNT       PIC 9(07).
               10  YDBH2-MARGIN            PIC 9(03)V9(06).
               10  YDBH2-DISCOUNT          PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
