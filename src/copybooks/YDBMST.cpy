      *================================================================*
      * COPYBOOK : YDBMST                                              *
      * PURPOSE  : DATE WINDOW BRIDGE MASTER RECORD (YDBMST KSDS)      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-19 LMB  ORIGINAL                                       *
      * 1992-11-23 TWH  NEW AMOUNT-1 REPORT COLUMNS INC9698            *
      * 1994-06-08 GAF  AMOUNT-1 NOTICE TEXT PER LEGAL PRB9328         *
      * 1997-02-23 BHS  AMOUNT-1 EXCEPTIONS TO OPS QUEUE REQ0350       *
      * 1998-10-18 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-07-14 RJT  CONTROL EXCEPTIONS TO OPS QUEUE CHG1475        *
      * 2000-12-16 MKL  AUDIT FINDING - AMOUNT-1 CONTROL TOTALS PRB6296*
      * 2001-03-22 JLM  AMOUNT-2 RERUN PROTECTION REQ8498              *
      * 2005-12-09 SKN  AMOUNT-2 TABLE EXPANDED PRB0538                *
      * 2007-05-05 BHS  CONTROL FEE SCHEDULE UPDATE PRB2419            *
      * 2008-04-01 SKN  AMOUNT-2 STATUS CODES ADDED INC8267            *
      *================================================================*
       01  YDB-MASTER-REC.
           05  YDBM-KEY                    PIC X(12).
           05  YDBM-CUST-ID                PIC X(10).
           05  YDBM-NAME                   PIC X(30).
           05  YDBM-DESC                   PIC X(30).
           05  YDBM-REF-NO                 PIC X(16).
           05  YDBM-ADDR                   PIC X(30).
           05  YDBM-CONTACT                PIC X(20).
           05  YDB-MASTER-AMOUNTS.
               10  YDBM-AMOUNT-1-AMT       PIC S9(13)V99 COMP-3.
               10  YDBM-AMOUNT-1-YTD       PIC S9(13)V99 COMP-3.
               10  YDBM-AMOUNT-2-AMT       PIC S9(13)V99 COMP-3.
               10  YDBM-AMOUNT-2-YTD       PIC S9(13)V99 COMP-3.
               10  YDBM-CONTROL-AMT        PIC S9(13)V99 COMP-3.
               10  YDBM-CONTROL-YTD        PIC S9(13)V99 COMP-3.
           05  YDB-MASTER-DATES.
               10  YDBM-OPEN-DATE          PIC 9(08).
               10  YDBM-LAST-MAINT-DATE    PIC 9(08).
               10  YDBM-NOTICE-DATE        PIC 9(08).
               10  YDBM-MATURITY-DATE      PIC 9(08).
               10  YDBM-NEXT-DUE-DATE      PIC 9(08).
               10  YDBM-LAST-ACT-DATE      PIC 9(08).
               10  YDBM-LAST-STMT-DATE     PIC 9(08).
               10  YDBM-EXPIRY-DATE        PIC 9(08).
               10  YDBM-LAST-PAY-DATE      PIC 9(08).
           05  YDB-MASTER-CODES.
               10  YDBM-WINDOW-RULE-CD     PIC X(01).
                   88  YDBM-WINDOW-RULE-CD-A       VALUE 'A'.
                   88  YDBM-WINDOW-RULE-CD-D       VALUE 'D'.
                   88  YDBM-WINDOW-RULE-CD-S       VALUE 'S'.
                   88  YDBM-WINDOW-RULE-CD-T       VALUE 'T'.
               10  YDBM-PIVOT-CD           PIC X(03).
                   88  YDBM-PIVOT-CD-KGT           VALUE 'KGT'.
                   88  YDBM-PIVOT-CD-NNG           VALUE 'NNG'.
                   88  YDBM-PIVOT-CD-PTC           VALUE 'PTC'.
                   88  YDBM-PIVOT-CD-WZW           VALUE 'WZW'.
               10  YDBM-REGION-CD          PIC X(01).
               10  YDBM-REASON-CD          PIC X(02).
                   88  YDBM-REASON-CD-AX           VALUE 'AX'.
                   88  YDBM-REASON-CD-LK           VALUE 'LK'.
                   88  YDBM-REASON-CD-NT           VALUE 'NT'.
                   88  YDBM-REASON-CD-UW           VALUE 'UW'.
               10  YDBM-SOURCE-CD          PIC X(03).
                   88  YDBM-SOURCE-CD-BNX          VALUE 'BNX'.
                   88  YDBM-SOURCE-CD-FTB          VALUE 'FTB'.
                   88  YDBM-SOURCE-CD-GAP          VALUE 'GAP'.
                   88  YDBM-SOURCE-CD-PAP          VALUE 'PAP'.
                   88  YDBM-SOURCE-CD-VGC          VALUE 'VGC'.
                   88  YDBM-SOURCE-CD-VJV          VALUE 'VJV'.
               10  YDBM-CYCLE-CD           PIC X(02).
                   88  YDBM-CYCLE-CD-FJ            VALUE 'FJ'.
                   88  YDBM-CYCLE-CD-LA            VALUE 'LA'.
                   88  YDBM-CYCLE-CD-SP            VALUE 'SP'.
                   88  YDBM-CYCLE-CD-UC            VALUE 'UC'.
               10  YDBM-REVIEW-FLAG        PIC X(01).
                   88  YDBM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  YDBM-NOTICE-SENT-FLAG   PIC X(01).
                   88  YDBM-NOTICE-SENT-FLAG-YES   VALUE 'Y'.
               10  YDBM-AUTO-RENEW-FLAG    PIC X(01).
                   88  YDBM-AUTO-RENEW-FLAG-YES    VALUE 'Y'.
               10  YDBM-PREFERRED-FLAG     PIC X(01).
                   88  YDBM-PREFERRED-FLAG-YES     VALUE 'Y'.
               10  YDBM-OVERRIDE-FLAG      PIC X(01).
                   88  YDBM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  YDBM-EXEMPT-FLAG        PIC X(01).
                   88  YDBM-EXEMPT-FLAG-YES        VALUE 'Y'.
           05  YDB-MASTER-COUNTERS.
               10  YDBM-ITEM-CNT           PIC 9(07).
               10  YDBM-PAYMENT-CNT        PIC 9(07).
               10  YDBM-RETRY-CNT          PIC 9(07).
               10  YDBM-CYCLE-CNT          PIC 9(07).
               10  YDBM-PENALTY-PCT        PIC 9(03)V9(06).
               10  YDBM-MARGIN             PIC 9(03)V9(06).
               10  YDBM-DISCOUNT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
