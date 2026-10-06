      *================================================================*
      * COPYBOOK : BURISKP                                             *
      * PURPOSE  : LINKAGE FOR BUMP-UP CERTIFICATES RULE MODULE BURISK *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-06 RJT  ORIGINAL                                       *
      * 1995-06-05 BHS  BUMP CUTOFF MOVED PRB0540                      *
      * 1995-09-21 SKN  ADDON STATUS CODES ADDED REQ7720               *
      * 2000-08-20 SKN  ADDON LIMIT REVISED PRB1340                    *
      * 2001-05-11 MKL  AUDIT FINDING - ADDON CONTROL TOTALS CHG3552   *
      * 2001-04-24 CRP  BUMP LIMIT REVISED REQ7563                     *
      * 2002-06-15 MKL  BUMP NOTICE TEXT PER LEGAL INC8069             *
      *================================================================*
       01  BURISK-PARMS.
           05  BURISK-PENALTY-YTD          PIC S9(13)V99 COMP-3.
           05  BURISK-INTEREST-YTD         PIC S9(13)V99 COMP-3.
           05  BURISK-ADDON-AMT            PIC S9(13)V99 COMP-3.
           05  BURISK-ADDON-YTD            PIC S9(13)V99 COMP-3.
           05  BURISK-PRINCIPAL-AMT        PIC S9(13)V99 COMP-3.
           05  BURISK-INTEREST-AMT         PIC S9(13)V99 COMP-3.
           05  BURISK-LAST-MAINT-DATE      PIC 9(08).
           05  BURISK-LAST-PAY-DATE        PIC 9(08).
           05  BURISK-NEXT-REVIEW-DATE     PIC 9(08).
           05  BURISK-LAST-STMT-DATE       PIC 9(08).
           05  BURISK-CYCLE-CD             PIC X(01).
               88  BURISK-CYCLE-CD-A               VALUE 'A'.
               88  BURISK-CYCLE-CD-S               VALUE 'S'.
               88  BURISK-CYCLE-CD-T               VALUE 'T'.
               88  BURISK-CYCLE-CD-U               VALUE 'U'.
               88  BURISK-CYCLE-CD-X               VALUE 'X'.
               88  BURISK-CYCLE-CD-Y               VALUE 'Y'.
           05  BURISK-ELECTION-CD          PIC X(01).
               88  BURISK-ELECTION-CD-L            VALUE 'L'.
               88  BURISK-ELECTION-CD-M            VALUE 'M'.
               88  BURISK-ELECTION-CD-N            VALUE 'N'.
               88  BURISK-ELECTION-CD-R            VALUE 'R'.
               88  BURISK-ELECTION-CD-V            VALUE 'V'.
           05  BURISK-REGION-CD            PIC X(03).
               88  BURISK-REGION-CD-BGJ            VALUE 'BGJ'.
               88  BURISK-REGION-CD-LFX            VALUE 'LFX'.
               88  BURISK-REGION-CD-MLA            VALUE 'MLA'.
               88  BURISK-REGION-CD-ULA            VALUE 'ULA'.
           05  BURISK-CHANNEL-CD           PIC X(02).
               88  BURISK-CHANNEL-CD-EK            VALUE 'EK'.
               88  BURISK-CHANNEL-CD-JE            VALUE 'JE'.
               88  BURISK-CHANNEL-CD-PN            VALUE 'PN'.
               88  BURISK-CHANNEL-CD-WV            VALUE 'WV'.
           05  BURISK-REVIEW-FLAG          PIC X(01).
               88  BURISK-REVIEW-FLAG-YES          VALUE 'Y'.
           05  BURISK-HOLD-FLAG            PIC X(01).
               88  BURISK-HOLD-FLAG-YES            VALUE 'Y'.
           05  BURISK-EXEMPT-FLAG          PIC X(01).
               88  BURISK-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  BURISK-EXCEPT-CNT           PIC 9(07).
           05  BURISK-CYCLE-CNT            PIC 9(07).
           05  BURISK-PENALTY-PCT          PIC 9(03)V9(06).
           05  BURISK-MARGIN               PIC 9(03)V9(06).
           05  BURISK-DESC                 PIC X(30).
           05  BURISK-CUST-ID              PIC X(10).
           05  BURISK-RETURN-CODE          PIC 9(02).
