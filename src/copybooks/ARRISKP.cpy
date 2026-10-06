      *================================================================*
      * COPYBOOK : ARRISKP                                             *
      * PURPOSE  : LINKAGE FOR ACCOUNT RECONCILEMENT RULE MODULE ARRISK*
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-12-21 TWH  ORIGINAL                                       *
      * 2004-03-18 PQR  FEE NOTICE TEXT PER LEGAL PRB1463              *
      * 2005-12-06 MKL  OUTSTAND TABLE EXPANDED CHG1184                *
      * 2006-10-16 PQR  PERFORMANCE - FEE READ AHEAD PRB1105           *
      * 2010-07-25 CRP  STOP CALCULATION CORRECTED INC6355             *
      * 2011-11-06 BHS  VOID CUTOFF MOVED PRB7731                      *
      *================================================================*
       01  ARRISK-PARMS.
           05  ARRISK-ISSUED-YTD           PIC S9(13)V99 COMP-3.
           05  ARRISK-PAID-YTD             PIC S9(13)V99 COMP-3.
           05  ARRISK-VOID-YTD             PIC S9(13)V99 COMP-3.
           05  ARRISK-STOP-YTD             PIC S9(13)V99 COMP-3.
           05  ARRISK-ISSUED-AMT           PIC S9(13)V99 COMP-3.
           05  ARRISK-OUTSTAND-YTD         PIC S9(13)V99 COMP-3.
           05  ARRISK-LAST-STMT-DATE       PIC 9(08).
           05  ARRISK-NEXT-REVIEW-DATE     PIC 9(08).
           05  ARRISK-LAST-PAY-DATE        PIC 9(08).
           05  ARRISK-NEXT-DUE-DATE        PIC 9(08).
           05  ARRISK-CYCLE-CD             PIC X(02).
               88  ARRISK-CYCLE-CD-CP              VALUE 'CP'.
               88  ARRISK-CYCLE-CD-MR              VALUE 'MR'.
               88  ARRISK-CYCLE-CD-PY              VALUE 'PY'.
               88  ARRISK-CYCLE-CD-SP              VALUE 'SP'.
               88  ARRISK-CYCLE-CD-TP              VALUE 'TP'.
           05  ARRISK-CHANNEL-CD           PIC X(02).
               88  ARRISK-CHANNEL-CD-ME            VALUE 'ME'.
               88  ARRISK-CHANNEL-CD-SW            VALUE 'SW'.
               88  ARRISK-CHANNEL-CD-UA            VALUE 'UA'.
               88  ARRISK-CHANNEL-CD-WB            VALUE 'WB'.
           05  ARRISK-REGION-CD            PIC X(02).
               88  ARRISK-REGION-CD-AD             VALUE 'AD'.
               88  ARRISK-REGION-CD-BU             VALUE 'BU'.
               88  ARRISK-REGION-CD-GM             VALUE 'GM'.
               88  ARRISK-REGION-CD-SW             VALUE 'SW'.
               88  ARRISK-REGION-CD-WD             VALUE 'WD'.
               88  ARRISK-REGION-CD-XV             VALUE 'XV'.
           05  ARRISK-BRANCH-CD            PIC X(02).
               88  ARRISK-BRANCH-CD-FN             VALUE 'FN'.
               88  ARRISK-BRANCH-CD-GM             VALUE 'GM'.
               88  ARRISK-BRANCH-CD-GV             VALUE 'GV'.
               88  ARRISK-BRANCH-CD-HK             VALUE 'HK'.
               88  ARRISK-BRANCH-CD-VX             VALUE 'VX'.
           05  ARRISK-NOTICE-SENT-FLAG     PIC X(01).
               88  ARRISK-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  ARRISK-STOP-FLAG            PIC X(01).
               88  ARRISK-STOP-FLAG-YES            VALUE 'Y'.
           05  ARRISK-HOLD-FLAG            PIC X(01).
               88  ARRISK-HOLD-FLAG-YES            VALUE 'Y'.
           05  ARRISK-ITEM-CNT             PIC 9(07).
           05  ARRISK-CYCLE-CNT            PIC 9(07).
           05  ARRISK-PENALTY-PCT          PIC 9(03)V9(06).
           05  ARRISK-DISCOUNT             PIC 9(03)V9(06).
           05  ARRISK-ADDR                 PIC X(30).
           05  ARRISK-DESC                 PIC X(30).
           05  ARRISK-RETURN-CODE          PIC 9(02).
