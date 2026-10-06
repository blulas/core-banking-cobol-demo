      *================================================================*
      * COPYBOOK : BUDTEP                                              *
      * PURPOSE  : LINKAGE FOR BUMP-UP CERTIFICATES RULE MODULE BUDTE  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-28 BHS  ORIGINAL                                       *
      * 1996-10-09 RJT  BUMP ROUNDING - HALF UP REQ5379                *
      * 1997-05-19 GAF  AUDIT FINDING - PRINCIPAL CONTROL TOTALS CHG765*
      * 2002-05-22 DPS  ADDON LIMIT REVISED PRB6548                    *
      * 2003-09-03 DPS  PENALTY EXCEPTIONS TO OPS QUEUE INC5103        *
      * 2003-11-08 MKL  PENALTY LIMIT REVISED INC6486                  *
      * 2005-01-09 JLM  INTEREST CUTOFF MOVED PRB1533                  *
      * 2005-01-16 PQR  INTEREST STATUS CODES ADDED INC2055            *
      * 2007-08-08 EWS  AUDIT FINDING - INTEREST CONTROL TOTALS REQ4808*
      *================================================================*
       01  BUDTE-PARMS.
           05  BUDTE-BUMP-YTD              PIC S9(13)V99 COMP-3.
           05  BUDTE-BUMP-AMT              PIC S9(13)V99 COMP-3.
           05  BUDTE-PRINCIPAL-YTD         PIC S9(13)V99 COMP-3.
           05  BUDTE-ADDON-AMT             PIC S9(13)V99 COMP-3.
           05  BUDTE-PENALTY-YTD           PIC S9(13)V99 COMP-3.
           05  BUDTE-INTEREST-YTD          PIC S9(13)V99 COMP-3.
           05  BUDTE-LAST-MAINT-DATE       PIC 9(08).
           05  BUDTE-LAST-PAY-DATE         PIC 9(08).
           05  BUDTE-EXPIRY-DATE           PIC 9(08).
           05  BUDTE-LAST-STMT-DATE        PIC 9(08).
           05  BUDTE-ELECTION-CD           PIC X(01).
               88  BUDTE-ELECTION-CD-L             VALUE 'L'.
               88  BUDTE-ELECTION-CD-M             VALUE 'M'.
               88  BUDTE-ELECTION-CD-N             VALUE 'N'.
               88  BUDTE-ELECTION-CD-R             VALUE 'R'.
               88  BUDTE-ELECTION-CD-V             VALUE 'V'.
           05  BUDTE-SOURCE-CD             PIC X(02).
               88  BUDTE-SOURCE-CD-AG              VALUE 'AG'.
               88  BUDTE-SOURCE-CD-BF              VALUE 'BF'.
               88  BUDTE-SOURCE-CD-HF              VALUE 'HF'.
               88  BUDTE-SOURCE-CD-HR              VALUE 'HR'.
               88  BUDTE-SOURCE-CD-UF              VALUE 'UF'.
               88  BUDTE-SOURCE-CD-YD              VALUE 'YD'.
           05  BUDTE-REGION-CD             PIC X(03).
               88  BUDTE-REGION-CD-BGJ             VALUE 'BGJ'.
               88  BUDTE-REGION-CD-LFX             VALUE 'LFX'.
               88  BUDTE-REGION-CD-MLA             VALUE 'MLA'.
               88  BUDTE-REGION-CD-ULA             VALUE 'ULA'.
           05  BUDTE-CYCLE-CD              PIC X(01).
               88  BUDTE-CYCLE-CD-A                VALUE 'A'.
               88  BUDTE-CYCLE-CD-S                VALUE 'S'.
               88  BUDTE-CYCLE-CD-T                VALUE 'T'.
               88  BUDTE-CYCLE-CD-U                VALUE 'U'.
               88  BUDTE-CYCLE-CD-X                VALUE 'X'.
               88  BUDTE-CYCLE-CD-Y                VALUE 'Y'.
           05  BUDTE-REVIEW-FLAG           PIC X(01).
               88  BUDTE-REVIEW-FLAG-YES           VALUE 'Y'.
           05  BUDTE-OVERRIDE-FLAG         PIC X(01).
               88  BUDTE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  BUDTE-SUSPEND-FLAG          PIC X(01).
               88  BUDTE-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  BUDTE-CYCLE-CNT             PIC 9(07).
           05  BUDTE-EXCEPT-CNT            PIC 9(07).
           05  BUDTE-MARGIN                PIC 9(03)V9(06).
           05  BUDTE-PENALTY-PCT           PIC 9(03)V9(06).
           05  BUDTE-NAME                  PIC X(30).
           05  BUDTE-DESC                  PIC X(30).
           05  BUDTE-RETURN-CODE           PIC 9(02).
