      *================================================================*
      * COPYBOOK : TLPENP                                              *
      * PURPOSE  : LINKAGE FOR TELLER BALANCING RULE MODULE TLPEN      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-04-24 MKL  ORIGINAL                                       *
      * 1997-01-14 MKL  CASH-IN RERUN PROTECTION PRB5878               *
      * 2000-07-05 BHS  VAULT-SELL TABLE EXPANDED REQ9223              *
      * 2000-10-21 GAF  CASH-OUT ROUNDING - HALF UP CHG9709            *
      * 2005-12-10 CRP  BATCH RERUN PROTECTION REQ6954                 *
      * 2006-03-12 DPS  AUDIT FINDING - VAULT-SELL CONTROL TOTALS CHG54*
      * 2006-04-28 GAF  PERFORMANCE - CASH-IN READ AHEAD INC1817       *
      *================================================================*
       01  TLPEN-PARMS.
           05  TLPEN-OVERSHORT-AMT         PIC S9(13)V99 COMP-3.
           05  TLPEN-VAULT-BUY-AMT         PIC S9(13)V99 COMP-3.
           05  TLPEN-CASH-IN-AMT           PIC S9(13)V99 COMP-3.
           05  TLPEN-CASH-OUT-YTD          PIC S9(13)V99 COMP-3.
           05  TLPEN-BATCH-AMT             PIC S9(13)V99 COMP-3.
           05  TLPEN-CASH-OUT-AMT          PIC S9(13)V99 COMP-3.
           05  TLPEN-OPEN-DATE             PIC 9(08).
           05  TLPEN-EFFECTIVE-DATE        PIC 9(08).
           05  TLPEN-NOTICE-DATE           PIC 9(08).
           05  TLPEN-NEXT-REVIEW-DATE      PIC 9(08).
           05  TLPEN-DRAWER-CD             PIC X(01).
               88  TLPEN-DRAWER-CD-D               VALUE 'D'.
               88  TLPEN-DRAWER-CD-F               VALUE 'F'.
               88  TLPEN-DRAWER-CD-R               VALUE 'R'.
           05  TLPEN-REASON-CD             PIC X(02).
               88  TLPEN-REASON-CD-GS              VALUE 'GS'.
               88  TLPEN-REASON-CD-GV              VALUE 'GV'.
               88  TLPEN-REASON-CD-PG              VALUE 'PG'.
               88  TLPEN-REASON-CD-SX              VALUE 'SX'.
               88  TLPEN-REASON-CD-XN              VALUE 'XN'.
               88  TLPEN-REASON-CD-YZ              VALUE 'YZ'.
           05  TLPEN-CHANNEL-CD            PIC X(01).
               88  TLPEN-CHANNEL-CD-D              VALUE 'D'.
               88  TLPEN-CHANNEL-CD-F              VALUE 'F'.
               88  TLPEN-CHANNEL-CD-M              VALUE 'M'.
               88  TLPEN-CHANNEL-CD-N              VALUE 'N'.
               88  TLPEN-CHANNEL-CD-R              VALUE 'R'.
               88  TLPEN-CHANNEL-CD-T              VALUE 'T'.
           05  TLPEN-SOURCE-CD             PIC X(02).
               88  TLPEN-SOURCE-CD-AF              VALUE 'AF'.
               88  TLPEN-SOURCE-CD-PT              VALUE 'PT'.
               88  TLPEN-SOURCE-CD-SZ              VALUE 'SZ'.
               88  TLPEN-SOURCE-CD-VK              VALUE 'VK'.
               88  TLPEN-SOURCE-CD-VW              VALUE 'VW'.
           05  TLPEN-SUSPEND-FLAG          PIC X(01).
               88  TLPEN-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  TLPEN-REVIEW-FLAG           PIC X(01).
               88  TLPEN-REVIEW-FLAG-YES           VALUE 'Y'.
           05  TLPEN-HOLD-FLAG             PIC X(01).
               88  TLPEN-HOLD-FLAG-YES             VALUE 'Y'.
           05  TLPEN-ITEM-CNT              PIC 9(07).
           05  TLPEN-RETRY-CNT             PIC 9(07).
           05  TLPEN-RATE                  PIC 9(03)V9(06).
           05  TLPEN-FEE-PCT               PIC 9(03)V9(06).
           05  TLPEN-REF-NO                PIC X(16).
           05  TLPEN-DESC                  PIC X(30).
           05  TLPEN-RETURN-CODE           PIC 9(02).
