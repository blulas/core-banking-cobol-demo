      *================================================================*
      * COPYBOOK : TLWAIVP                                             *
      * PURPOSE  : LINKAGE FOR TELLER BALANCING RULE MODULE TLWAIV     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1990-04-22 EWS  ORIGINAL                                       *
      * 1997-05-05 JLM  OVERSHORT TABLE EXPANDED CHG2830               *
      * 1997-03-15 CRP  AUDIT FINDING - CASH-IN CONTROL TOTALS CHG2884 *
      * 1998-04-20 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-10-07 GAF  CASH-IN RERUN PROTECTION REQ4308               *
      * 2002-10-11 MKL  OVERSHORT EXCEPTIONS TO OPS QUEUE INC6506      *
      * 2010-10-19 MKL  CASH-OUT ROUNDING - HALF UP REQ1156            *
      *================================================================*
       01  TLWAIV-PARMS.
           05  TLWAIV-VAULT-SELL-AMT       PIC S9(13)V99 COMP-3.
           05  TLWAIV-VAULT-BUY-YTD        PIC S9(13)V99 COMP-3.
           05  TLWAIV-BATCH-YTD            PIC S9(13)V99 COMP-3.
           05  TLWAIV-CASH-IN-AMT          PIC S9(13)V99 COMP-3.
           05  TLWAIV-CASH-IN-YTD          PIC S9(13)V99 COMP-3.
           05  TLWAIV-CASH-OUT-YTD         PIC S9(13)V99 COMP-3.
           05  TLWAIV-EFFECTIVE-DATE       PIC 9(08).
           05  TLWAIV-LAST-STMT-DATE       PIC 9(08).
           05  TLWAIV-LAST-PAY-DATE        PIC 9(08).
           05  TLWAIV-OPEN-DATE            PIC 9(08).
           05  TLWAIV-SOURCE-CD            PIC X(02).
               88  TLWAIV-SOURCE-CD-AF             VALUE 'AF'.
               88  TLWAIV-SOURCE-CD-PT             VALUE 'PT'.
               88  TLWAIV-SOURCE-CD-SZ             VALUE 'SZ'.
               88  TLWAIV-SOURCE-CD-VK             VALUE 'VK'.
               88  TLWAIV-SOURCE-CD-VW             VALUE 'VW'.
           05  TLWAIV-DRAWER-CD            PIC X(01).
               88  TLWAIV-DRAWER-CD-D              VALUE 'D'.
               88  TLWAIV-DRAWER-CD-F              VALUE 'F'.
               88  TLWAIV-DRAWER-CD-R              VALUE 'R'.
           05  TLWAIV-CHANNEL-CD           PIC X(01).
               88  TLWAIV-CHANNEL-CD-D             VALUE 'D'.
               88  TLWAIV-CHANNEL-CD-F             VALUE 'F'.
               88  TLWAIV-CHANNEL-CD-M             VALUE 'M'.
               88  TLWAIV-CHANNEL-CD-N             VALUE 'N'.
               88  TLWAIV-CHANNEL-CD-R             VALUE 'R'.
               88  TLWAIV-CHANNEL-CD-T             VALUE 'T'.
           05  TLWAIV-REASON-CD            PIC X(02).
               88  TLWAIV-REASON-CD-GS             VALUE 'GS'.
               88  TLWAIV-REASON-CD-GV             VALUE 'GV'.
               88  TLWAIV-REASON-CD-PG             VALUE 'PG'.
               88  TLWAIV-REASON-CD-SX             VALUE 'SX'.
               88  TLWAIV-REASON-CD-XN             VALUE 'XN'.
               88  TLWAIV-REASON-CD-YZ             VALUE 'YZ'.
           05  TLWAIV-STOP-FLAG            PIC X(01).
               88  TLWAIV-STOP-FLAG-YES            VALUE 'Y'.
           05  TLWAIV-SUSPEND-FLAG         PIC X(01).
               88  TLWAIV-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  TLWAIV-EXEMPT-FLAG          PIC X(01).
               88  TLWAIV-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  TLWAIV-ITEM-CNT             PIC 9(07).
           05  TLWAIV-EXCEPT-CNT           PIC 9(07).
           05  TLWAIV-FEE-PCT              PIC 9(03)V9(06).
           05  TLWAIV-RATE                 PIC 9(03)V9(06).
           05  TLWAIV-DESC                 PIC X(30).
           05  TLWAIV-REF-NO               PIC X(16).
           05  TLWAIV-RETURN-CODE          PIC 9(02).
