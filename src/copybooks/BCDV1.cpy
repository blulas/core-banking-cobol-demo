      *================================================================*
      * COPYBOOK : BCDV1                                               *
      * PURPOSE  : BUMP-UP CERTIFICATES ARCHIVE RECORD LAYOUT          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-05-22 LMB  ORIGINAL                                       *
      * 1997-09-28 RJT  ADDON EXCEPTIONS TO OPS QUEUE INC2772          *
      * 1998-05-15 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2001-03-25 GAF  NEW PENALTY REPORT COLUMNS PRB5567             *
      * 2002-08-27 LMB  PENALTY CUTOFF MOVED REQ1560                   *
      * 2002-11-15 TWH  PENALTY TABLE EXPANDED INC7142                 *
      * 2005-08-24 LMB  PENALTY STATUS CODES ADDED CHG2602             *
      * 2005-07-05 DPS  PENALTY ROUNDING - HALF UP CHG5962             *
      * 2006-04-10 JLM  INTEREST TABLE EXPANDED PRB0262                *
      * 2006-02-28 TWH  PRINCIPAL NOTICE TEXT PER LEGAL PRB3115        *
      * 2006-05-17 MKL  NEW BUMP REPORT COLUMNS REQ4200                *
      * 2007-04-22 PQR  ADDON TABLE EXPANDED INC0433                   *
      *================================================================*
       01  BCD-H1-REC.
           05  BCDH1-CONTACT               PIC X(20).
           05  BCDH1-DESC                  PIC X(30).
           05  BCDH1-NAME                  PIC X(30).
           05  BCDH1-KEY                   PIC X(12).
           05  BCDH1-ADDR                  PIC X(30).
           05  BCDH1-REF-NO                PIC X(16).
           05  BCD-H1-AMOUNTS.
               10  BCDH1-PENALTY-AMT       PIC S9(13)V99 COMP-3.
               10  BCDH1-PRINCIPAL-AMT     PIC S9(13)V99 COMP-3.
               10  BCDH1-PRINCIPAL-YTD     PIC S9(13)V99 COMP-3.
               10  BCDH1-ADDON-YTD         PIC S9(13)V99 COMP-3.
               10  BCDH1-INTEREST-YTD      PIC S9(13)V99 COMP-3.
               10  BCDH1-ADDON-AMT         PIC S9(13)V99 COMP-3.
               10  BCDH1-PENALTY-YTD       PIC S9(13)V99 COMP-3.
               10  BCDH1-BUMP-YTD          PIC S9(13)V99 COMP-3.
               10  BCDH1-BUMP-AMT          PIC S9(13)V99 COMP-3.
               10  BCDH1-INTEREST-AMT      PIC S9(13)V99 COMP-3.
           05  BCD-H1-DATES.
               10  BCDH1-MATURITY-DATE     PIC 9(08).
               10  BCDH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  BCDH1-LAST-STMT-DATE    PIC 9(08).
               10  BCDH1-NEXT-DUE-DATE     PIC 9(08).
               10  BCDH1-CLOSE-DATE        PIC 9(08).
               10  BCDH1-EXPIRY-DATE       PIC 9(08).
               10  BCDH1-LAST-MAINT-DATE   PIC 9(08).
               10  BCDH1-LAST-PAY-DATE     PIC 9(08).
               10  BCDH1-EFFECTIVE-DATE    PIC 9(08).
           05  BCD-H1-CODES.
               10  BCDH1-CHANNEL-CD        PIC X(02).
                   88  BCDH1-CHANNEL-CD-EK         VALUE 'EK'.
                   88  BCDH1-CHANNEL-CD-JE         VALUE 'JE'.
                   88  BCDH1-CHANNEL-CD-PN         VALUE 'PN'.
                   88  BCDH1-CHANNEL-CD-WV         VALUE 'WV'.
               10  BCDH1-STOP-FLAG         PIC X(01).
                   88  BCDH1-STOP-FLAG-YES         VALUE 'Y'.
               10  BCDH1-OVERRIDE-FLAG     PIC X(01).
                   88  BCDH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  BCDH1-REGION-CD         PIC X(03).
                   88  BCDH1-REGION-CD-BGJ         VALUE 'BGJ'.
                   88  BCDH1-REGION-CD-LFX         VALUE 'LFX'.
                   88  BCDH1-REGION-CD-MLA         VALUE 'MLA'.
                   88  BCDH1-REGION-CD-ULA         VALUE 'ULA'.
               10  BCDH1-CYCLE-CD          PIC X(01).
                   88  BCDH1-CYCLE-CD-A            VALUE 'A'.
                   88  BCDH1-CYCLE-CD-S            VALUE 'S'.
                   88  BCDH1-CYCLE-CD-T            VALUE 'T'.
                   88  BCDH1-CYCLE-CD-U            VALUE 'U'.
                   88  BCDH1-CYCLE-CD-X            VALUE 'X'.
                   88  BCDH1-CYCLE-CD-Y            VALUE 'Y'.
               10  BCDH1-SUSPEND-FLAG      PIC X(01).
                   88  BCDH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  BCDH1-EXEMPT-FLAG       PIC X(01).
                   88  BCDH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  BCDH1-SOURCE-CD         PIC X(02).
                   88  BCDH1-SOURCE-CD-AG          VALUE 'AG'.
                   88  BCDH1-SOURCE-CD-BF          VALUE 'BF'.
                   88  BCDH1-SOURCE-CD-HF          VALUE 'HF'.
                   88  BCDH1-SOURCE-CD-HR          VALUE 'HR'.
                   88  BCDH1-SOURCE-CD-UF          VALUE 'UF'.
                   88  BCDH1-SOURCE-CD-YD          VALUE 'YD'.
               10  BCDH1-STEP-PLAN-CD      PIC X(03).
               10  BCDH1-REVIEW-FLAG       PIC X(01).
                   88  BCDH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  BCDH1-HOLD-FLAG         PIC X(01).
                   88  BCDH1-HOLD-FLAG-YES         VALUE 'Y'.
           05  BCD-H1-COUNTERS.
               10  BCDH1-RETRY-CNT         PIC 9(07).
               10  BCDH1-CYCLE-CNT         PIC 9(07).
               10  BCDH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  BCDH1-EXCEPT-CNT        PIC 9(07).
               10  BCDH1-ITEM-CNT          PIC 9(07).
               10  BCDH1-MARGIN            PIC 9(03)V9(06).
               10  BCDH1-RATE              PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
