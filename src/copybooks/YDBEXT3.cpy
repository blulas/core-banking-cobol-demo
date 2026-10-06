      *================================================================*
      * COPYBOOK : YDBEXT3                                             *
      * PURPOSE  : DATE WINDOW BRIDGE EXTRACT RECORD LAYOUT            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-06-19 DPS  ORIGINAL                                       *
      * 1993-08-01 LMB  AMOUNT-1 STATUS CODES ADDED REQ7619            *
      * 1994-05-04 JLM  AMOUNT-1 CUTOFF MOVED REQ2604                  *
      * 1995-04-23 EWS  AMOUNT-1 EXCEPTIONS TO OPS QUEUE REQ6566       *
      * 1995-05-21 DPS  AMOUNT-1 FEE SCHEDULE UPDATE CHG1819           *
      * 1999-06-04 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-07-15 DPS  NEW AMOUNT-1 REPORT COLUMNS INC9267            *
      * 2004-11-12 CRP  AMOUNT-1 STATUS CODES ADDED INC8454            *
      * 2007-11-10 BHS  AMOUNT-1 ROUNDING - HALF UP CHG7019            *
      * 2008-12-01 SKN  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS REQ3789*
      *================================================================*
       01  YDB-H3-REC.
           05  YDBH3-CONTACT               PIC X(20).
           05  YDBH3-REF-NO                PIC X(16).
           05  YDBH3-DESC                  PIC X(30).
           05  YDBH3-KEY                   PIC X(12).
           05  YDBH3-CUST-ID               PIC X(10).
           05  YDBH3-NAME                  PIC X(30).
           05  YDBH3-ADDR                  PIC X(30).
           05  YDB-H3-AMOUNTS.
               10  YDBH3-AMOUNT-2-YTD      PIC S9(13)V99 COMP-3.
               10  YDBH3-CONTROL-YTD       PIC S9(13)V99 COMP-3.
               10  YDBH3-AMOUNT-2-AMT      PIC S9(13)V99 COMP-3.
               10  YDBH3-AMOUNT-1-AMT      PIC S9(13)V99 COMP-3.
               10  YDBH3-AMOUNT-1-YTD      PIC S9(13)V99 COMP-3.
               10  YDBH3-CONTROL-AMT       PIC S9(13)V99 COMP-3.
           05  YDB-H3-DATES.
               10  YDBH3-LAST-ACT-DATE     PIC 9(08).
               10  YDBH3-LAST-STMT-DATE    PIC 9(08).
               10  YDBH3-NEXT-DUE-DATE     PIC 9(08).
               10  YDBH3-LAST-MAINT-DATE   PIC 9(08).
               10  YDBH3-NOTICE-DATE       PIC 9(08).
           05  YDB-H3-CODES.
               10  YDBH3-CYCLE-CD          PIC X(02).
                   88  YDBH3-CYCLE-CD-FJ           VALUE 'FJ'.
                   88  YDBH3-CYCLE-CD-LA           VALUE 'LA'.
                   88  YDBH3-CYCLE-CD-SP           VALUE 'SP'.
                   88  YDBH3-CYCLE-CD-UC           VALUE 'UC'.
               10  YDBH3-NOTICE-SENT-FLAG  PIC X(01).
                   88  YDBH3-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  YDBH3-PIVOT-CD          PIC X(03).
                   88  YDBH3-PIVOT-CD-KGT          VALUE 'KGT'.
                   88  YDBH3-PIVOT-CD-NNG          VALUE 'NNG'.
                   88  YDBH3-PIVOT-CD-PTC          VALUE 'PTC'.
                   88  YDBH3-PIVOT-CD-WZW          VALUE 'WZW'.
               10  YDBH3-AUTO-RENEW-FLAG   PIC X(01).
                   88  YDBH3-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  YDBH3-OVERRIDE-FLAG     PIC X(01).
                   88  YDBH3-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  YDBH3-SOURCE-CD         PIC X(03).
                   88  YDBH3-SOURCE-CD-BNX         VALUE 'BNX'.
                   88  YDBH3-SOURCE-CD-FTB         VALUE 'FTB'.
                   88  YDBH3-SOURCE-CD-GAP         VALUE 'GAP'.
                   88  YDBH3-SOURCE-CD-PAP         VALUE 'PAP'.
                   88  YDBH3-SOURCE-CD-VGC         VALUE 'VGC'.
                   88  YDBH3-SOURCE-CD-VJV         VALUE 'VJV'.
               10  YDBH3-REGION-CD         PIC X(01).
               10  YDBH3-WINDOW-RULE-CD    PIC X(01).
                   88  YDBH3-WINDOW-RULE-CD-A      VALUE 'A'.
                   88  YDBH3-WINDOW-RULE-CD-D      VALUE 'D'.
                   88  YDBH3-WINDOW-RULE-CD-S      VALUE 'S'.
                   88  YDBH3-WINDOW-RULE-CD-T      VALUE 'T'.
               10  YDBH3-EXEMPT-FLAG       PIC X(01).
                   88  YDBH3-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  YDBH3-REASON-CD         PIC X(02).
                   88  YDBH3-REASON-CD-AX          VALUE 'AX'.
                   88  YDBH3-REASON-CD-LK          VALUE 'LK'.
                   88  YDBH3-REASON-CD-NT          VALUE 'NT'.
                   88  YDBH3-REASON-CD-UW          VALUE 'UW'.
               10  YDBH3-PREFERRED-FLAG    PIC X(01).
                   88  YDBH3-PREFERRED-FLAG-YES    VALUE 'Y'.
           05  YDB-H3-COUNTERS.
               10  YDBH3-RETRY-CNT         PIC 9(07).
               10  YDBH3-DISCOUNT          PIC 9(03)V9(06).
               10  YDBH3-ITEM-CNT          PIC 9(07).
               10  YDBH3-CYCLE-CNT         PIC 9(07).
               10  YDBH3-PENALTY-PCT       PIC 9(03)V9(06).
               10  YDBH3-PAYMENT-CNT       PIC 9(07).
           05  FILLER                      PIC X(20).
