      *================================================================*
      * COPYBOOK : BCDMST                                              *
      * PURPOSE  : BUMP-UP CERTIFICATES MASTER RECORD (BCDMST KSDS)    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-06-25 JLM  ORIGINAL                                       *
      * 1993-09-16 BHS  PENALTY CALCULATION CORRECTED INC7904          *
      * 1996-03-06 MKL  AUDIT FINDING - PRINCIPAL CONTROL TOTALS INC094*
      * 1996-04-16 BHS  INTEREST ROUNDING - HALF UP CHG2479            *
      * 1996-02-17 SKN  PRINCIPAL CALCULATION CORRECTED PRB4361        *
      * 1999-01-22 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-02-05 JLM  PENALTY LIMIT REVISED REQ5975                  *
      * 2003-04-07 EWS  NEW PRINCIPAL REPORT COLUMNS INC2058           *
      * 2004-07-21 TWH  PENALTY RERUN PROTECTION INC9212               *
      * 2005-03-23 CRP  PRINCIPAL LIMIT REVISED REQ8010                *
      * 2006-06-16 BHS  PENALTY NOTICE TEXT PER LEGAL PRB2014          *
      *================================================================*
       01  BCD-MASTER-REC.
           05  BCDM-KEY                    PIC X(12).
           05  BCDM-CUST-ID                PIC X(10).
           05  BCDM-NAME                   PIC X(30).
           05  BCDM-DESC                   PIC X(30).
           05  BCDM-REF-NO                 PIC X(16).
           05  BCDM-ADDR                   PIC X(30).
           05  BCDM-CONTACT                PIC X(20).
           05  BCD-MASTER-AMOUNTS.
               10  BCDM-PRINCIPAL-AMT      PIC S9(13)V99 COMP-3.
               10  BCDM-PRINCIPAL-YTD      PIC S9(13)V99 COMP-3.
               10  BCDM-INTEREST-AMT       PIC S9(13)V99 COMP-3.
               10  BCDM-INTEREST-YTD       PIC S9(13)V99 COMP-3.
               10  BCDM-BUMP-AMT           PIC S9(13)V99 COMP-3.
               10  BCDM-BUMP-YTD           PIC S9(13)V99 COMP-3.
               10  BCDM-PENALTY-AMT        PIC S9(13)V99 COMP-3.
               10  BCDM-PENALTY-YTD        PIC S9(13)V99 COMP-3.
               10  BCDM-ADDON-AMT          PIC S9(13)V99 COMP-3.
               10  BCDM-ADDON-YTD          PIC S9(13)V99 COMP-3.
           05  BCD-MASTER-DATES.
               10  BCDM-NEXT-REVIEW-DATE   PIC 9(08).
               10  BCDM-CLOSE-DATE         PIC 9(08).
               10  BCDM-LAST-MAINT-DATE    PIC 9(08).
               10  BCDM-LAST-STMT-DATE     PIC 9(08).
               10  BCDM-EFFECTIVE-DATE     PIC 9(08).
               10  BCDM-MATURITY-DATE      PIC 9(08).
               10  BCDM-EXPIRY-DATE        PIC 9(08).
               10  BCDM-LAST-PAY-DATE      PIC 9(08).
               10  BCDM-NEXT-DUE-DATE      PIC 9(08).
           05  BCD-MASTER-CODES.
               10  BCDM-STEP-PLAN-CD       PIC X(03).
               10  BCDM-ELECTION-CD        PIC X(01).
                   88  BCDM-ELECTION-CD-L          VALUE 'L'.
                   88  BCDM-ELECTION-CD-M          VALUE 'M'.
                   88  BCDM-ELECTION-CD-N          VALUE 'N'.
                   88  BCDM-ELECTION-CD-R          VALUE 'R'.
                   88  BCDM-ELECTION-CD-V          VALUE 'V'.
               10  BCDM-CHANNEL-CD         PIC X(02).
                   88  BCDM-CHANNEL-CD-EK          VALUE 'EK'.
                   88  BCDM-CHANNEL-CD-JE          VALUE 'JE'.
                   88  BCDM-CHANNEL-CD-PN          VALUE 'PN'.
                   88  BCDM-CHANNEL-CD-WV          VALUE 'WV'.
               10  BCDM-REGION-CD          PIC X(03).
                   88  BCDM-REGION-CD-BGJ          VALUE 'BGJ'.
                   88  BCDM-REGION-CD-LFX          VALUE 'LFX'.
                   88  BCDM-REGION-CD-MLA          VALUE 'MLA'.
                   88  BCDM-REGION-CD-ULA          VALUE 'ULA'.
               10  BCDM-CYCLE-CD           PIC X(01).
                   88  BCDM-CYCLE-CD-A             VALUE 'A'.
                   88  BCDM-CYCLE-CD-S             VALUE 'S'.
                   88  BCDM-CYCLE-CD-T             VALUE 'T'.
                   88  BCDM-CYCLE-CD-U             VALUE 'U'.
                   88  BCDM-CYCLE-CD-X             VALUE 'X'.
                   88  BCDM-CYCLE-CD-Y             VALUE 'Y'.
               10  BCDM-SOURCE-CD          PIC X(02).
                   88  BCDM-SOURCE-CD-AG           VALUE 'AG'.
                   88  BCDM-SOURCE-CD-BF           VALUE 'BF'.
                   88  BCDM-SOURCE-CD-HF           VALUE 'HF'.
                   88  BCDM-SOURCE-CD-HR           VALUE 'HR'.
                   88  BCDM-SOURCE-CD-UF           VALUE 'UF'.
                   88  BCDM-SOURCE-CD-YD           VALUE 'YD'.
               10  BCDM-HOLD-FLAG          PIC X(01).
                   88  BCDM-HOLD-FLAG-YES          VALUE 'Y'.
               10  BCDM-OVERRIDE-FLAG      PIC X(01).
                   88  BCDM-OVERRIDE-FLAG-YES      VALUE 'Y'.
               10  BCDM-SUSPEND-FLAG       PIC X(01).
                   88  BCDM-SUSPEND-FLAG-YES       VALUE 'Y'.
               10  BCDM-EXEMPT-FLAG        PIC X(01).
                   88  BCDM-EXEMPT-FLAG-YES        VALUE 'Y'.
               10  BCDM-REVIEW-FLAG        PIC X(01).
                   88  BCDM-REVIEW-FLAG-YES        VALUE 'Y'.
               10  BCDM-STOP-FLAG          PIC X(01).
                   88  BCDM-STOP-FLAG-YES          VALUE 'Y'.
           05  BCD-MASTER-COUNTERS.
               10  BCDM-RETRY-CNT          PIC 9(07).
               10  BCDM-ITEM-CNT           PIC 9(07).
               10  BCDM-CYCLE-CNT          PIC 9(07).
               10  BCDM-EXCEPT-CNT         PIC 9(07).
               10  BCDM-MARGIN             PIC 9(03)V9(06).
               10  BCDM-PENALTY-PCT        PIC 9(03)V9(06).
               10  BCDM-RATE               PIC 9(03)V9(06).
           05  FILLER                      PIC X(60).
