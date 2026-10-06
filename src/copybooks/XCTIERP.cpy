      *================================================================*
      * COPYBOOK : XCTIERP                                             *
      * PURPOSE  : LINKAGE FOR CHRISTMAS CLUB SAVINGS RULE MODULE      *
      *            XCTIER                                              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-06-02 DPS  ORIGINAL                                       *
      * 1995-02-11 CRP  WEEKLY STATUS CODES ADDED PRB0486              *
      * 1996-05-24 LMB  PAYOUT RERUN PROTECTION PRB7069                *
      * 2000-02-18 RJT  CONTRIB CUTOFF MOVED CHG4935                   *
      * 2000-08-17 PQR  BONUS RERUN PROTECTION REQ1418                 *
      *================================================================*
       01  XCTIER-PARMS.
           05  XCTIER-BONUS-YTD            PIC S9(13)V99 COMP-3.
           05  XCTIER-ACCRUED-AMT          PIC S9(13)V99 COMP-3.
           05  XCTIER-PAYOUT-AMT           PIC S9(13)V99 COMP-3.
           05  XCTIER-ACCRUED-YTD          PIC S9(13)V99 COMP-3.
           05  XCTIER-MISSED-AMT           PIC S9(13)V99 COMP-3.
           05  XCTIER-GOAL-AMT             PIC S9(13)V99 COMP-3.
           05  XCTIER-NOTICE-DATE          PIC 9(08).
           05  XCTIER-LAST-PAY-DATE        PIC 9(08).
           05  XCTIER-LAST-STMT-DATE       PIC 9(08).
           05  XCTIER-EXPIRY-DATE          PIC 9(08).
           05  XCTIER-PAYOUT-METHOD-CD     PIC X(01).
               88  XCTIER-PAYOUT-METHOD-CD-B       VALUE 'B'.
               88  XCTIER-PAYOUT-METHOD-CD-H       VALUE 'H'.
               88  XCTIER-PAYOUT-METHOD-CD-U       VALUE 'U'.
               88  XCTIER-PAYOUT-METHOD-CD-W       VALUE 'W'.
           05  XCTIER-REGION-CD            PIC X(03).
               88  XCTIER-REGION-CD-FLM            VALUE 'FLM'.
               88  XCTIER-REGION-CD-HLS            VALUE 'HLS'.
               88  XCTIER-REGION-CD-KMJ            VALUE 'KMJ'.
               88  XCTIER-REGION-CD-PDA            VALUE 'PDA'.
               88  XCTIER-REGION-CD-RVG            VALUE 'RVG'.
               88  XCTIER-REGION-CD-WMB            VALUE 'WMB'.
           05  XCTIER-PLAN-CD              PIC X(02).
           05  XCTIER-REASON-CD            PIC X(02).
           05  XCTIER-OVERRIDE-FLAG        PIC X(01).
               88  XCTIER-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  XCTIER-SUSPEND-FLAG         PIC X(01).
               88  XCTIER-SUSPEND-FLAG-YES         VALUE 'Y'.
           05  XCTIER-PREFERRED-FLAG       PIC X(01).
               88  XCTIER-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  XCTIER-RETRY-CNT            PIC 9(07).
           05  XCTIER-CYCLE-CNT            PIC 9(07).
           05  XCTIER-RATE                 PIC 9(03)V9(06).
           05  XCTIER-DISCOUNT             PIC 9(03)V9(06).
           05  XCTIER-NAME                 PIC X(30).
           05  XCTIER-CONTACT              PIC X(20).
           05  XCTIER-RETURN-CODE          PIC 9(02).
