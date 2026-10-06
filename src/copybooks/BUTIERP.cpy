      *================================================================*
      * COPYBOOK : BUTIERP                                             *
      * PURPOSE  : LINKAGE FOR BUMP-UP CERTIFICATES RULE MODULE BUTIER *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-03-16 BHS  ORIGINAL                                       *
      * 1994-05-09 GAF  ADDON RERUN PROTECTION CHG9331                 *
      * 1995-11-20 JLM  INTEREST NOTICE TEXT PER LEGAL INC4644         *
      * 1996-12-10 PQR  PENALTY EXCEPTIONS TO OPS QUEUE PRB8380        *
      * 1998-04-23 GAF  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-06-03 RJT  PERFORMANCE - ADDON READ AHEAD INC8483         *
      * 1999-10-08 TWH  INTEREST CUTOFF MOVED CHG6851                  *
      * 2000-11-07 CRP  INTEREST ROUNDING - HALF UP INC0578            *
      * 2005-01-20 EWS  NEW BUMP REPORT COLUMNS CHG1249                *
      * 2007-10-21 DPS  PERFORMANCE - ADDON READ AHEAD INC1286         *
      *================================================================*
       01  BUTIER-PARMS.
           05  BUTIER-PENALTY-YTD          PIC S9(13)V99 COMP-3.
           05  BUTIER-ADDON-AMT            PIC S9(13)V99 COMP-3.
           05  BUTIER-BUMP-AMT             PIC S9(13)V99 COMP-3.
           05  BUTIER-PRINCIPAL-AMT        PIC S9(13)V99 COMP-3.
           05  BUTIER-INTEREST-AMT         PIC S9(13)V99 COMP-3.
           05  BUTIER-PRINCIPAL-YTD        PIC S9(13)V99 COMP-3.
           05  BUTIER-NEXT-DUE-DATE        PIC 9(08).
           05  BUTIER-CLOSE-DATE           PIC 9(08).
           05  BUTIER-EFFECTIVE-DATE       PIC 9(08).
           05  BUTIER-MATURITY-DATE        PIC 9(08).
           05  BUTIER-REGION-CD            PIC X(03).
               88  BUTIER-REGION-CD-BGJ            VALUE 'BGJ'.
               88  BUTIER-REGION-CD-LFX            VALUE 'LFX'.
               88  BUTIER-REGION-CD-MLA            VALUE 'MLA'.
               88  BUTIER-REGION-CD-ULA            VALUE 'ULA'.
           05  BUTIER-CYCLE-CD             PIC X(01).
               88  BUTIER-CYCLE-CD-A               VALUE 'A'.
               88  BUTIER-CYCLE-CD-S               VALUE 'S'.
               88  BUTIER-CYCLE-CD-T               VALUE 'T'.
               88  BUTIER-CYCLE-CD-U               VALUE 'U'.
               88  BUTIER-CYCLE-CD-X               VALUE 'X'.
               88  BUTIER-CYCLE-CD-Y               VALUE 'Y'.
           05  BUTIER-CHANNEL-CD           PIC X(02).
               88  BUTIER-CHANNEL-CD-EK            VALUE 'EK'.
               88  BUTIER-CHANNEL-CD-JE            VALUE 'JE'.
               88  BUTIER-CHANNEL-CD-PN            VALUE 'PN'.
               88  BUTIER-CHANNEL-CD-WV            VALUE 'WV'.
           05  BUTIER-SOURCE-CD            PIC X(02).
               88  BUTIER-SOURCE-CD-AG             VALUE 'AG'.
               88  BUTIER-SOURCE-CD-BF             VALUE 'BF'.
               88  BUTIER-SOURCE-CD-HF             VALUE 'HF'.
               88  BUTIER-SOURCE-CD-HR             VALUE 'HR'.
               88  BUTIER-SOURCE-CD-UF             VALUE 'UF'.
               88  BUTIER-SOURCE-CD-YD             VALUE 'YD'.
           05  BUTIER-REVIEW-FLAG          PIC X(01).
               88  BUTIER-REVIEW-FLAG-YES          VALUE 'Y'.
           05  BUTIER-EXEMPT-FLAG          PIC X(01).
               88  BUTIER-EXEMPT-FLAG-YES          VALUE 'Y'.
           05  BUTIER-OVERRIDE-FLAG        PIC X(01).
               88  BUTIER-OVERRIDE-FLAG-YES        VALUE 'Y'.
           05  BUTIER-EXCEPT-CNT           PIC 9(07).
           05  BUTIER-RETRY-CNT            PIC 9(07).
           05  BUTIER-MARGIN               PIC 9(03)V9(06).
           05  BUTIER-RATE                 PIC 9(03)V9(06).
           05  BUTIER-CUST-ID              PIC X(10).
           05  BUTIER-REF-NO               PIC X(16).
           05  BUTIER-RETURN-CODE          PIC 9(02).
