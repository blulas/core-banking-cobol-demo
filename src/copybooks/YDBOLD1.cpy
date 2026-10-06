      *================================================================*
      * COPYBOOK : YDBOLD1                                             *
      * PURPOSE  : DATE WINDOW BRIDGE HISTORY RECORD LAYOUT            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-09-24 BHS  ORIGINAL                                       *
      * 1997-07-05 JLM  AMOUNT-1 CALCULATION CORRECTED PRB0666         *
      * 1998-08-01 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-09-27 LMB  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS REQ7366*
      * 2004-01-12 JLM  NEW AMOUNT-1 REPORT COLUMNS INC7718            *
      * 2005-08-08 CRP  CONTROL LIMIT REVISED PRB1399                  *
      * 2006-12-09 TWH  AUDIT FINDING - AMOUNT-2 CONTROL TOTALS REQ6087*
      *================================================================*
       01  YDB-H1-REC.
           05  YDBH1-CUST-ID               PIC X(10).
           05  YDBH1-ADDR                  PIC X(30).
           05  YDBH1-KEY                   PIC X(12).
           05  YDBH1-CONTACT               PIC X(20).
           05  YDBH1-REF-NO                PIC X(16).
           05  YDB-H1-AMOUNTS.
               10  YDBH1-AMOUNT-2-YTD      PIC S9(13)V99 COMP-3.
               10  YDBH1-AMOUNT-1-YTD      PIC S9(13)V99 COMP-3.
               10  YDBH1-CONTROL-AMT       PIC S9(13)V99 COMP-3.
               10  YDBH1-AMOUNT-2-AMT      PIC S9(13)V99 COMP-3.
               10  YDBH1-CONTROL-YTD       PIC S9(13)V99 COMP-3.
               10  YDBH1-AMOUNT-1-AMT      PIC S9(13)V99 COMP-3.
           05  YDB-H1-DATES.
               10  YDBH1-LAST-MAINT-DATE   PIC 9(08).
               10  YDBH1-MATURITY-DATE     PIC 9(08).
               10  YDBH1-NOTICE-DATE       PIC 9(08).
               10  YDBH1-EXPIRY-DATE       PIC 9(08).
               10  YDBH1-LAST-STMT-DATE    PIC 9(08).
               10  YDBH1-LAST-ACT-DATE     PIC 9(08).
               10  YDBH1-LAST-PAY-DATE     PIC 9(08).
               10  YDBH1-OPEN-DATE         PIC 9(08).
           05  YDB-H1-CODES.
               10  YDBH1-SOURCE-CD         PIC X(03).
                   88  YDBH1-SOURCE-CD-BNX         VALUE 'BNX'.
                   88  YDBH1-SOURCE-CD-FTB         VALUE 'FTB'.
                   88  YDBH1-SOURCE-CD-GAP         VALUE 'GAP'.
                   88  YDBH1-SOURCE-CD-PAP         VALUE 'PAP'.
                   88  YDBH1-SOURCE-CD-VGC         VALUE 'VGC'.
                   88  YDBH1-SOURCE-CD-VJV         VALUE 'VJV'.
               10  YDBH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  YDBH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  YDBH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  YDBH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  YDBH1-PREFERRED-FLAG    PIC X(01).
                   88  YDBH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  YDBH1-WINDOW-RULE-CD    PIC X(01).
                   88  YDBH1-WINDOW-RULE-CD-A      VALUE 'A'.
                   88  YDBH1-WINDOW-RULE-CD-D      VALUE 'D'.
                   88  YDBH1-WINDOW-RULE-CD-S      VALUE 'S'.
                   88  YDBH1-WINDOW-RULE-CD-T      VALUE 'T'.
               10  YDBH1-REVIEW-FLAG       PIC X(01).
                   88  YDBH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  YDBH1-PIVOT-CD          PIC X(03).
                   88  YDBH1-PIVOT-CD-KGT          VALUE 'KGT'.
                   88  YDBH1-PIVOT-CD-NNG          VALUE 'NNG'.
                   88  YDBH1-PIVOT-CD-PTC          VALUE 'PTC'.
                   88  YDBH1-PIVOT-CD-WZW          VALUE 'WZW'.
               10  YDBH1-REASON-CD         PIC X(02).
                   88  YDBH1-REASON-CD-AX          VALUE 'AX'.
                   88  YDBH1-REASON-CD-LK          VALUE 'LK'.
                   88  YDBH1-REASON-CD-NT          VALUE 'NT'.
                   88  YDBH1-REASON-CD-UW          VALUE 'UW'.
               10  YDBH1-REGION-CD         PIC X(01).
               10  YDBH1-CYCLE-CD          PIC X(02).
                   88  YDBH1-CYCLE-CD-FJ           VALUE 'FJ'.
                   88  YDBH1-CYCLE-CD-LA           VALUE 'LA'.
                   88  YDBH1-CYCLE-CD-SP           VALUE 'SP'.
                   88  YDBH1-CYCLE-CD-UC           VALUE 'UC'.
               10  YDBH1-OVERRIDE-FLAG     PIC X(01).
                   88  YDBH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  YDBH1-EXEMPT-FLAG       PIC X(01).
                   88  YDBH1-EXEMPT-FLAG-YES       VALUE 'Y'.
           05  YDB-H1-COUNTERS.
               10  YDBH1-CYCLE-CNT         PIC 9(07).
               10  YDBH1-RETRY-CNT         PIC 9(07).
               10  YDBH1-PAYMENT-CNT       PIC 9(07).
               10  YDBH1-ITEM-CNT          PIC 9(07).
               10  YDBH1-MARGIN            PIC 9(03)V9(06).
               10  YDBH1-PENALTY-PCT       PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
