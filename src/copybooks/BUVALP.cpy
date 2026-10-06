      *================================================================*
      * COPYBOOK : BUVALP                                              *
      * PURPOSE  : LINKAGE FOR BUMP-UP CERTIFICATES RULE MODULE BUVAL  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-02-05 BHS  ORIGINAL                                       *
      * 1995-07-02 PQR  BUMP STATUS CODES ADDED CHG7707                *
      * 2002-11-23 JLM  AUDIT FINDING - BUMP CONTROL TOTALS INC0546    *
      * 2004-01-10 GAF  BUMP CUTOFF MOVED CHG9982                      *
      * 2004-06-12 TWH  NEW INTEREST REPORT COLUMNS CHG6915            *
      * 2005-07-24 GAF  PERFORMANCE - ADDON READ AHEAD CHG3186         *
      * 2005-12-21 EWS  BUMP CALCULATION CORRECTED CHG9488             *
      * 2006-05-20 LMB  NEW INTEREST REPORT COLUMNS REQ8004            *
      * 2006-05-05 JLM  ADDON NOTICE TEXT PER LEGAL INC7940            *
      *================================================================*
       01  BUVAL-PARMS.
           05  BUVAL-PENALTY-AMT           PIC S9(13)V99 COMP-3.
           05  BUVAL-INTEREST-YTD          PIC S9(13)V99 COMP-3.
           05  BUVAL-BUMP-AMT              PIC S9(13)V99 COMP-3.
           05  BUVAL-PRINCIPAL-YTD         PIC S9(13)V99 COMP-3.
           05  BUVAL-ADDON-YTD             PIC S9(13)V99 COMP-3.
           05  BUVAL-PRINCIPAL-AMT         PIC S9(13)V99 COMP-3.
           05  BUVAL-LAST-STMT-DATE        PIC 9(08).
           05  BUVAL-LAST-MAINT-DATE       PIC 9(08).
           05  BUVAL-EFFECTIVE-DATE        PIC 9(08).
           05  BUVAL-LAST-PAY-DATE         PIC 9(08).
           05  BUVAL-STEP-PLAN-CD          PIC X(03).
           05  BUVAL-CHANNEL-CD            PIC X(02).
               88  BUVAL-CHANNEL-CD-EK             VALUE 'EK'.
               88  BUVAL-CHANNEL-CD-JE             VALUE 'JE'.
               88  BUVAL-CHANNEL-CD-PN             VALUE 'PN'.
               88  BUVAL-CHANNEL-CD-WV             VALUE 'WV'.
           05  BUVAL-SOURCE-CD             PIC X(02).
               88  BUVAL-SOURCE-CD-AG              VALUE 'AG'.
               88  BUVAL-SOURCE-CD-BF              VALUE 'BF'.
               88  BUVAL-SOURCE-CD-HF              VALUE 'HF'.
               88  BUVAL-SOURCE-CD-HR              VALUE 'HR'.
               88  BUVAL-SOURCE-CD-UF              VALUE 'UF'.
               88  BUVAL-SOURCE-CD-YD              VALUE 'YD'.
           05  BUVAL-REGION-CD             PIC X(03).
               88  BUVAL-REGION-CD-BGJ             VALUE 'BGJ'.
               88  BUVAL-REGION-CD-LFX             VALUE 'LFX'.
               88  BUVAL-REGION-CD-MLA             VALUE 'MLA'.
               88  BUVAL-REGION-CD-ULA             VALUE 'ULA'.
           05  BUVAL-EXEMPT-FLAG           PIC X(01).
               88  BUVAL-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  BUVAL-STOP-FLAG             PIC X(01).
               88  BUVAL-STOP-FLAG-YES             VALUE 'Y'.
           05  BUVAL-OVERRIDE-FLAG         PIC X(01).
               88  BUVAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  BUVAL-CYCLE-CNT             PIC 9(07).
           05  BUVAL-ITEM-CNT              PIC 9(07).
           05  BUVAL-RATE                  PIC 9(03)V9(06).
           05  BUVAL-MARGIN                PIC 9(03)V9(06).
           05  BUVAL-DESC                  PIC X(30).
           05  BUVAL-NAME                  PIC X(30).
           05  BUVAL-RETURN-CODE           PIC 9(02).
