      *================================================================*
      * COPYBOOK : TBLV3                                               *
      * PURPOSE  : TELLER BALANCING ARCHIVE RECORD LAYOUT              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-05-07 GAF  ORIGINAL                                       *
      * 1996-06-15 DPS  NEW CASH-IN REPORT COLUMNS REQ9319             *
      * 2001-02-23 LMB  VAULT-BUY CALCULATION CORRECTED REQ7540        *
      * 2001-03-20 BHS  BATCH STATUS CODES ADDED REQ2619               *
      * 2002-09-09 GAF  NEW CASH-IN REPORT COLUMNS CHG8923             *
      * 2005-08-21 GAF  VAULT-BUY FEE SCHEDULE UPDATE PRB3948          *
      * 2009-10-09 BHS  VAULT-SELL STATUS CODES ADDED CHG9529          *
      * 2009-01-21 PQR  VAULT-BUY LIMIT REVISED CHG8542                *
      * 2009-04-24 JLM  VAULT-BUY ROUNDING - HALF UP PRB8840           *
      * 2010-10-02 TWH  OVERSHORT NOTICE TEXT PER LEGAL CHG1753        *
      * 2010-03-25 CRP  BATCH STATUS CODES ADDED REQ2457               *
      * 2011-05-11 CRP  CASH-IN RERUN PROTECTION CHG2870               *
      * 2011-01-16 TWH  NEW CASH-OUT REPORT COLUMNS INC0978            *
      *================================================================*
       01  TBL-H3-REC.
           05  TBLH3-ADDR                  PIC X(30).
           05  TBLH3-REF-NO                PIC X(16).
           05  TBLH3-NAME                  PIC X(30).
           05  TBLH3-KEY                   PIC X(12).
           05  TBLH3-CONTACT               PIC X(20).
           05  TBLH3-DESC                  PIC X(30).
           05  TBLH3-CUST-ID               PIC X(10).
           05  TBL-H3-AMOUNTS.
               10  TBLH3-CASH-IN-AMT       PIC S9(13)V99 COMP-3.
               10  TBLH3-CASH-OUT-AMT      PIC S9(13)V99 COMP-3.
               10  TBLH3-VAULT-SELL-YTD    PIC S9(13)V99 COMP-3.
               10  TBLH3-OVERSHORT-AMT     PIC S9(13)V99 COMP-3.
               10  TBLH3-VAULT-BUY-YTD     PIC S9(13)V99 COMP-3.
               10  TBLH3-BATCH-AMT         PIC S9(13)V99 COMP-3.
               10  TBLH3-CASH-OUT-YTD      PIC S9(13)V99 COMP-3.
               10  TBLH3-VAULT-SELL-AMT    PIC S9(13)V99 COMP-3.
               10  TBLH3-BATCH-YTD         PIC S9(13)V99 COMP-3.
               10  TBLH3-CASH-IN-YTD       PIC S9(13)V99 COMP-3.
               10  TBLH3-OVERSHORT-YTD     PIC S9(13)V99 COMP-3.
           05  TBL-H3-DATES.
               10  TBLH3-LAST-PAY-DATE     PIC 9(08).
               10  TBLH3-LAST-STMT-DATE    PIC 9(08).
               10  TBLH3-LAST-ACT-DATE     PIC 9(08).
               10  TBLH3-NEXT-REVIEW-DATE  PIC 9(08).
               10  TBLH3-NOTICE-DATE       PIC 9(08).
               10  TBLH3-OPEN-DATE         PIC 9(08).
               10  TBLH3-EXPIRY-DATE       PIC 9(08).
               10  TBLH3-LAST-MAINT-DATE   PIC 9(08).
           05  TBL-H3-CODES.
               10  TBLH3-REVIEW-FLAG       PIC X(01).
                   88  TBLH3-REVIEW-FLAG-YES       VALUE 'Y'.
               10  TBLH3-DRAWER-CD         PIC X(01).
                   88  TBLH3-DRAWER-CD-D           VALUE 'D'.
                   88  TBLH3-DRAWER-CD-F           VALUE 'F'.
                   88  TBLH3-DRAWER-CD-R           VALUE 'R'.
               10  TBLH3-SOURCE-CD         PIC X(02).
                   88  TBLH3-SOURCE-CD-AF          VALUE 'AF'.
                   88  TBLH3-SOURCE-CD-PT          VALUE 'PT'.
                   88  TBLH3-SOURCE-CD-SZ          VALUE 'SZ'.
                   88  TBLH3-SOURCE-CD-VK          VALUE 'VK'.
                   88  TBLH3-SOURCE-CD-VW          VALUE 'VW'.
               10  TBLH3-STOP-FLAG         PIC X(01).
                   88  TBLH3-STOP-FLAG-YES         VALUE 'Y'.
               10  TBLH3-OVERRIDE-FLAG     PIC X(01).
                   88  TBLH3-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  TBLH3-EXEMPT-FLAG       PIC X(01).
                   88  TBLH3-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  TBLH3-REASON-CD         PIC X(02).
                   88  TBLH3-REASON-CD-GS          VALUE 'GS'.
                   88  TBLH3-REASON-CD-GV          VALUE 'GV'.
                   88  TBLH3-REASON-CD-PG          VALUE 'PG'.
                   88  TBLH3-REASON-CD-SX          VALUE 'SX'.
                   88  TBLH3-REASON-CD-XN          VALUE 'XN'.
                   88  TBLH3-REASON-CD-YZ          VALUE 'YZ'.
               10  TBLH3-SUSPEND-FLAG      PIC X(01).
                   88  TBLH3-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  TBLH3-CYCLE-CD          PIC X(02).
                   88  TBLH3-CYCLE-CD-KX           VALUE 'KX'.
                   88  TBLH3-CYCLE-CD-TA           VALUE 'TA'.
                   88  TBLH3-CYCLE-CD-TG           VALUE 'TG'.
                   88  TBLH3-CYCLE-CD-WG           VALUE 'WG'.
               10  TBLH3-PROOF-TYPE-CD     PIC X(02).
                   88  TBLH3-PROOF-TYPE-CD-JG      VALUE 'JG'.
                   88  TBLH3-PROOF-TYPE-CD-RX      VALUE 'RX'.
                   88  TBLH3-PROOF-TYPE-CD-TW      VALUE 'TW'.
                   88  TBLH3-PROOF-TYPE-CD-UG      VALUE 'UG'.
                   88  TBLH3-PROOF-TYPE-CD-XY      VALUE 'XY'.
               10  TBLH3-HOLD-FLAG         PIC X(01).
                   88  TBLH3-HOLD-FLAG-YES         VALUE 'Y'.
           05  TBL-H3-COUNTERS.
               10  TBLH3-RATE              PIC 9(03)V9(06).
               10  TBLH3-EXCEPT-CNT        PIC 9(07).
               10  TBLH3-FEE-PCT           PIC 9(03)V9(06).
               10  TBLH3-DISCOUNT          PIC 9(03)V9(06).
               10  TBLH3-NOTICE-CNT        PIC 9(07).
               10  TBLH3-RETRY-CNT         PIC 9(07).
               10  TBLH3-ITEM-CNT          PIC 9(07).
           05  FILLER                      PIC X(20).
