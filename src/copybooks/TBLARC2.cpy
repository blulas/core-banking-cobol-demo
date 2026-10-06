      *================================================================*
      * COPYBOOK : TBLARC2                                             *
      * PURPOSE  : TELLER BALANCING ARCHIVE RECORD LAYOUT              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-07-25 BHS  ORIGINAL                                       *
      * 1993-01-27 RJT  VAULT-BUY NOTICE TEXT PER LEGAL INC4479        *
      * 1994-01-06 CRP  CASH-IN EXCEPTIONS TO OPS QUEUE CHG8503        *
      * 1998-07-02 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-12-11 MKL  CASH-OUT CUTOFF MOVED CHG9649                  *
      * 2005-05-06 DPS  BATCH NOTICE TEXT PER LEGAL CHG5380            *
      * 2006-07-13 CRP  CASH-OUT CALCULATION CORRECTED PRB5810         *
      * 2006-10-08 DPS  CASH-OUT LIMIT REVISED PRB7560                 *
      *================================================================*
       01  TBL-H2-REC.
           05  TBLH2-CONTACT               PIC X(20).
           05  TBLH2-REF-NO                PIC X(16).
           05  TBLH2-DESC                  PIC X(30).
           05  TBLH2-ADDR                  PIC X(30).
           05  TBLH2-CUST-ID               PIC X(10).
           05  TBLH2-KEY                   PIC X(12).
           05  TBLH2-NAME                  PIC X(30).
           05  TBL-H2-AMOUNTS.
               10  TBLH2-VAULT-SELL-AMT    PIC S9(13)V99 COMP-3.
               10  TBLH2-CASH-IN-YTD       PIC S9(13)V99 COMP-3.
               10  TBLH2-BATCH-AMT         PIC S9(13)V99 COMP-3.
               10  TBLH2-VAULT-BUY-AMT     PIC S9(13)V99 COMP-3.
               10  TBLH2-VAULT-BUY-YTD     PIC S9(13)V99 COMP-3.
               10  TBLH2-CASH-OUT-AMT      PIC S9(13)V99 COMP-3.
               10  TBLH2-BATCH-YTD         PIC S9(13)V99 COMP-3.
               10  TBLH2-CASH-IN-AMT       PIC S9(13)V99 COMP-3.
               10  TBLH2-OVERSHORT-YTD     PIC S9(13)V99 COMP-3.
               10  TBLH2-OVERSHORT-AMT     PIC S9(13)V99 COMP-3.
               10  TBLH2-VAULT-SELL-YTD    PIC S9(13)V99 COMP-3.
               10  TBLH2-CASH-OUT-YTD      PIC S9(13)V99 COMP-3.
           05  TBL-H2-DATES.
               10  TBLH2-EFFECTIVE-DATE    PIC 9(08).
               10  TBLH2-NOTICE-DATE       PIC 9(08).
               10  TBLH2-LAST-STMT-DATE    PIC 9(08).
               10  TBLH2-LAST-ACT-DATE     PIC 9(08).
               10  TBLH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  TBLH2-LAST-PAY-DATE     PIC 9(08).
               10  TBLH2-LAST-MAINT-DATE   PIC 9(08).
               10  TBLH2-OPEN-DATE         PIC 9(08).
               10  TBLH2-EXPIRY-DATE       PIC 9(08).
           05  TBL-H2-CODES.
               10  TBLH2-CYCLE-CD          PIC X(02).
                   88  TBLH2-CYCLE-CD-KX           VALUE 'KX'.
                   88  TBLH2-CYCLE-CD-TA           VALUE 'TA'.
                   88  TBLH2-CYCLE-CD-TG           VALUE 'TG'.
                   88  TBLH2-CYCLE-CD-WG           VALUE 'WG'.
               10  TBLH2-EXEMPT-FLAG       PIC X(01).
                   88  TBLH2-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  TBLH2-PROOF-TYPE-CD     PIC X(02).
                   88  TBLH2-PROOF-TYPE-CD-JG      VALUE 'JG'.
                   88  TBLH2-PROOF-TYPE-CD-RX      VALUE 'RX'.
                   88  TBLH2-PROOF-TYPE-CD-TW      VALUE 'TW'.
                   88  TBLH2-PROOF-TYPE-CD-UG      VALUE 'UG'.
                   88  TBLH2-PROOF-TYPE-CD-XY      VALUE 'XY'.
               10  TBLH2-OVERRIDE-FLAG     PIC X(01).
                   88  TBLH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  TBLH2-DRAWER-CD         PIC X(01).
                   88  TBLH2-DRAWER-CD-D           VALUE 'D'.
                   88  TBLH2-DRAWER-CD-F           VALUE 'F'.
                   88  TBLH2-DRAWER-CD-R           VALUE 'R'.
               10  TBLH2-SUSPEND-FLAG      PIC X(01).
                   88  TBLH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  TBLH2-CHANNEL-CD        PIC X(01).
                   88  TBLH2-CHANNEL-CD-D          VALUE 'D'.
                   88  TBLH2-CHANNEL-CD-F          VALUE 'F'.
                   88  TBLH2-CHANNEL-CD-M          VALUE 'M'.
                   88  TBLH2-CHANNEL-CD-N          VALUE 'N'.
                   88  TBLH2-CHANNEL-CD-R          VALUE 'R'.
                   88  TBLH2-CHANNEL-CD-T          VALUE 'T'.
               10  TBLH2-HOLD-FLAG         PIC X(01).
                   88  TBLH2-HOLD-FLAG-YES         VALUE 'Y'.
               10  TBLH2-STOP-FLAG         PIC X(01).
                   88  TBLH2-STOP-FLAG-YES         VALUE 'Y'.
               10  TBLH2-REASON-CD         PIC X(02).
                   88  TBLH2-REASON-CD-GS          VALUE 'GS'.
                   88  TBLH2-REASON-CD-GV          VALUE 'GV'.
                   88  TBLH2-REASON-CD-PG          VALUE 'PG'.
                   88  TBLH2-REASON-CD-SX          VALUE 'SX'.
                   88  TBLH2-REASON-CD-XN          VALUE 'XN'.
                   88  TBLH2-REASON-CD-YZ          VALUE 'YZ'.
           05  TBL-H2-COUNTERS.
               10  TBLH2-NOTICE-CNT        PIC 9(07).
               10  TBLH2-DISCOUNT          PIC 9(03)V9(06).
               10  TBLH2-FEE-PCT           PIC 9(03)V9(06).
               10  TBLH2-ITEM-CNT          PIC 9(07).
               10  TBLH2-EXCEPT-CNT        PIC 9(07).
               10  TBLH2-RATE              PIC 9(03)V9(06).
               10  TBLH2-RETRY-CNT         PIC 9(07).
           05  FILLER                      PIC X(20).
