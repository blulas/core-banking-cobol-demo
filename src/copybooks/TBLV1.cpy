      *================================================================*
      * COPYBOOK : TBLV1                                               *
      * PURPOSE  : TELLER BALANCING ARCHIVE RECORD LAYOUT              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-05-07 BHS  ORIGINAL                                       *
      * 1993-07-24 DPS  CASH-OUT NOTICE TEXT PER LEGAL CHG5334         *
      * 1993-08-21 LMB  CASH-OUT LIMIT REVISED INC2524                 *
      * 1998-05-19 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-19 EWS  CASH-IN CALCULATION CORRECTED CHG8668          *
      * 2005-12-21 CRP  PERFORMANCE - CASH-IN READ AHEAD PRB6661       *
      * 2005-08-20 SKN  CASH-IN CUTOFF MOVED REQ4388                   *
      * 2010-07-12 EWS  CASH-OUT TABLE EXPANDED PRB1884                *
      * 2011-09-24 TWH  OVERSHORT RERUN PROTECTION PRB3732             *
      * 2012-11-09 RJT  CASH-OUT STATUS CODES ADDED REQ8115            *
      * 2012-10-10 LMB  OVERSHORT NOTICE TEXT PER LEGAL PRB8678        *
      *================================================================*
       01  TBL-H1-REC.
           05  TBLH1-KEY                   PIC X(12).
           05  TBLH1-CONTACT               PIC X(20).
           05  TBLH1-NAME                  PIC X(30).
           05  TBLH1-DESC                  PIC X(30).
           05  TBLH1-ADDR                  PIC X(30).
           05  TBLH1-REF-NO                PIC X(16).
           05  TBL-H1-AMOUNTS.
               10  TBLH1-VAULT-SELL-AMT    PIC S9(13)V99 COMP-3.
               10  TBLH1-VAULT-BUY-AMT     PIC S9(13)V99 COMP-3.
               10  TBLH1-BATCH-YTD         PIC S9(13)V99 COMP-3.
               10  TBLH1-CASH-OUT-AMT      PIC S9(13)V99 COMP-3.
               10  TBLH1-VAULT-BUY-YTD     PIC S9(13)V99 COMP-3.
               10  TBLH1-CASH-IN-AMT       PIC S9(13)V99 COMP-3.
               10  TBLH1-OVERSHORT-YTD     PIC S9(13)V99 COMP-3.
               10  TBLH1-CASH-IN-YTD       PIC S9(13)V99 COMP-3.
               10  TBLH1-CASH-OUT-YTD      PIC S9(13)V99 COMP-3.
               10  TBLH1-VAULT-SELL-YTD    PIC S9(13)V99 COMP-3.
               10  TBLH1-OVERSHORT-AMT     PIC S9(13)V99 COMP-3.
               10  TBLH1-BATCH-AMT         PIC S9(13)V99 COMP-3.
           05  TBL-H1-DATES.
               10  TBLH1-OPEN-DATE         PIC 9(08).
               10  TBLH1-LAST-STMT-DATE    PIC 9(08).
               10  TBLH1-NOTICE-DATE       PIC 9(08).
               10  TBLH1-LAST-ACT-DATE     PIC 9(08).
               10  TBLH1-LAST-PAY-DATE     PIC 9(08).
               10  TBLH1-EXPIRY-DATE       PIC 9(08).
               10  TBLH1-EFFECTIVE-DATE    PIC 9(08).
               10  TBLH1-LAST-MAINT-DATE   PIC 9(08).
               10  TBLH1-NEXT-REVIEW-DATE  PIC 9(08).
           05  TBL-H1-CODES.
               10  TBLH1-DRAWER-CD         PIC X(01).
                   88  TBLH1-DRAWER-CD-D           VALUE 'D'.
                   88  TBLH1-DRAWER-CD-F           VALUE 'F'.
                   88  TBLH1-DRAWER-CD-R           VALUE 'R'.
               10  TBLH1-EXEMPT-FLAG       PIC X(01).
                   88  TBLH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  TBLH1-OVERRIDE-FLAG     PIC X(01).
                   88  TBLH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  TBLH1-STOP-FLAG         PIC X(01).
                   88  TBLH1-STOP-FLAG-YES         VALUE 'Y'.
               10  TBLH1-SOURCE-CD         PIC X(02).
                   88  TBLH1-SOURCE-CD-AF          VALUE 'AF'.
                   88  TBLH1-SOURCE-CD-PT          VALUE 'PT'.
                   88  TBLH1-SOURCE-CD-SZ          VALUE 'SZ'.
                   88  TBLH1-SOURCE-CD-VK          VALUE 'VK'.
                   88  TBLH1-SOURCE-CD-VW          VALUE 'VW'.
               10  TBLH1-CYCLE-CD          PIC X(02).
                   88  TBLH1-CYCLE-CD-KX           VALUE 'KX'.
                   88  TBLH1-CYCLE-CD-TA           VALUE 'TA'.
                   88  TBLH1-CYCLE-CD-TG           VALUE 'TG'.
                   88  TBLH1-CYCLE-CD-WG           VALUE 'WG'.
               10  TBLH1-PROOF-TYPE-CD     PIC X(02).
                   88  TBLH1-PROOF-TYPE-CD-JG      VALUE 'JG'.
                   88  TBLH1-PROOF-TYPE-CD-RX      VALUE 'RX'.
                   88  TBLH1-PROOF-TYPE-CD-TW      VALUE 'TW'.
                   88  TBLH1-PROOF-TYPE-CD-UG      VALUE 'UG'.
                   88  TBLH1-PROOF-TYPE-CD-XY      VALUE 'XY'.
               10  TBLH1-CHANNEL-CD        PIC X(01).
                   88  TBLH1-CHANNEL-CD-D          VALUE 'D'.
                   88  TBLH1-CHANNEL-CD-F          VALUE 'F'.
                   88  TBLH1-CHANNEL-CD-M          VALUE 'M'.
                   88  TBLH1-CHANNEL-CD-N          VALUE 'N'.
                   88  TBLH1-CHANNEL-CD-R          VALUE 'R'.
                   88  TBLH1-CHANNEL-CD-T          VALUE 'T'.
               10  TBLH1-SUSPEND-FLAG      PIC X(01).
                   88  TBLH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  TBLH1-REASON-CD         PIC X(02).
                   88  TBLH1-REASON-CD-GS          VALUE 'GS'.
                   88  TBLH1-REASON-CD-GV          VALUE 'GV'.
                   88  TBLH1-REASON-CD-PG          VALUE 'PG'.
                   88  TBLH1-REASON-CD-SX          VALUE 'SX'.
                   88  TBLH1-REASON-CD-XN          VALUE 'XN'.
                   88  TBLH1-REASON-CD-YZ          VALUE 'YZ'.
               10  TBLH1-REVIEW-FLAG       PIC X(01).
                   88  TBLH1-REVIEW-FLAG-YES       VALUE 'Y'.
           05  TBL-H1-COUNTERS.
               10  TBLH1-ITEM-CNT          PIC 9(07).
               10  TBLH1-NOTICE-CNT        PIC 9(07).
               10  TBLH1-RATE              PIC 9(03)V9(06).
               10  TBLH1-EXCEPT-CNT        PIC 9(07).
               10  TBLH1-DISCOUNT          PIC 9(03)V9(06).
               10  TBLH1-FEE-PCT           PIC 9(03)V9(06).
               10  TBLH1-RETRY-CNT         PIC 9(07).
           05  FILLER                      PIC X(20).
