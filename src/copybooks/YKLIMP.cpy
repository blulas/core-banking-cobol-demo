      *================================================================*
      * COPYBOOK : YKLIMP                                              *
      * PURPOSE  : LINKAGE FOR DATE WINDOW BRIDGE RULE MODULE YKLIM    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-04-19 RJT  ORIGINAL                                       *
      * 1997-10-21 GAF  AMOUNT-1 CALCULATION CORRECTED PRB4391         *
      * 1997-03-22 DPS  AMOUNT-2 FEE SCHEDULE UPDATE PRB5147           *
      * 1997-02-21 GAF  NEW AMOUNT-2 REPORT COLUMNS INC4441            *
      * 1998-09-28 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-11-06 TWH  AMOUNT-1 LIMIT REVISED PRB7495                 *
      * 2001-01-26 BHS  CONTROL CUTOFF MOVED PRB2545                   *
      * 2002-09-05 LMB  AMOUNT-2 TABLE EXPANDED REQ5197                *
      *================================================================*
       01  YKLIM-PARMS.
           05  YKLIM-AMOUNT-2-AMT          PIC S9(13)V99 COMP-3.
           05  YKLIM-AMOUNT-1-YTD          PIC S9(13)V99 COMP-3.
           05  YKLIM-CONTROL-AMT           PIC S9(13)V99 COMP-3.
           05  YKLIM-AMOUNT-1-AMT          PIC S9(13)V99 COMP-3.
           05  YKLIM-AMOUNT-2-YTD          PIC S9(13)V99 COMP-3.
           05  YKLIM-CONTROL-YTD           PIC S9(13)V99 COMP-3.
           05  YKLIM-LAST-MAINT-DATE       PIC 9(08).
           05  YKLIM-OPEN-DATE             PIC 9(08).
           05  YKLIM-LAST-PAY-DATE         PIC 9(08).
           05  YKLIM-LAST-STMT-DATE        PIC 9(08).
           05  YKLIM-PIVOT-CD              PIC X(03).
               88  YKLIM-PIVOT-CD-KGT              VALUE 'KGT'.
               88  YKLIM-PIVOT-CD-NNG              VALUE 'NNG'.
               88  YKLIM-PIVOT-CD-PTC              VALUE 'PTC'.
               88  YKLIM-PIVOT-CD-WZW              VALUE 'WZW'.
           05  YKLIM-REGION-CD             PIC X(01).
           05  YKLIM-SOURCE-CD             PIC X(03).
               88  YKLIM-SOURCE-CD-BNX             VALUE 'BNX'.
               88  YKLIM-SOURCE-CD-FTB             VALUE 'FTB'.
               88  YKLIM-SOURCE-CD-GAP             VALUE 'GAP'.
               88  YKLIM-SOURCE-CD-PAP             VALUE 'PAP'.
               88  YKLIM-SOURCE-CD-VGC             VALUE 'VGC'.
               88  YKLIM-SOURCE-CD-VJV             VALUE 'VJV'.
           05  YKLIM-WINDOW-RULE-CD        PIC X(01).
               88  YKLIM-WINDOW-RULE-CD-A          VALUE 'A'.
               88  YKLIM-WINDOW-RULE-CD-D          VALUE 'D'.
               88  YKLIM-WINDOW-RULE-CD-S          VALUE 'S'.
               88  YKLIM-WINDOW-RULE-CD-T          VALUE 'T'.
           05  YKLIM-PREFERRED-FLAG        PIC X(01).
               88  YKLIM-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  YKLIM-OVERRIDE-FLAG         PIC X(01).
               88  YKLIM-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  YKLIM-NOTICE-SENT-FLAG      PIC X(01).
               88  YKLIM-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  YKLIM-CYCLE-CNT             PIC 9(07).
           05  YKLIM-PAYMENT-CNT           PIC 9(07).
           05  YKLIM-PENALTY-PCT           PIC 9(03)V9(06).
           05  YKLIM-DISCOUNT              PIC 9(03)V9(06).
           05  YKLIM-DESC                  PIC X(30).
           05  YKLIM-CONTACT               PIC X(20).
           05  YKLIM-RETURN-CODE           PIC 9(02).
