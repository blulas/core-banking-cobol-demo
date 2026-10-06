      *================================================================*
      * COPYBOOK : SDBHST3                                             *
      * PURPOSE  : SAFE DEPOSIT BOXES EXTRACT RECORD LAYOUT            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-12-13 LMB  ORIGINAL                                       *
      * 1998-07-04 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-28 JLM  PREPAID RERUN PROTECTION PRB2441               *
      * 2000-02-08 PQR  LATE-FEE TABLE EXPANDED PRB2951                *
      * 2000-03-11 GAF  LATE-FEE CALCULATION CORRECTED INC8422         *
      * 2000-02-13 LMB  RENT EXCEPTIONS TO OPS QUEUE REQ1297           *
      * 2001-12-10 GAF  DRILL EXCEPTIONS TO OPS QUEUE INC2098          *
      * 2003-04-19 BHS  KEY-FEE LIMIT REVISED CHG1781                  *
      * 2005-04-15 BHS  LATE-FEE EXCEPTIONS TO OPS QUEUE PRB4124       *
      * 2005-09-26 DPS  PERFORMANCE - RENT READ AHEAD PRB2374          *
      * 2006-01-14 RJT  LATE-FEE TABLE EXPANDED REQ8763                *
      * 2007-01-01 PQR  LATE-FEE CUTOFF MOVED PRB5211                  *
      * 2008-08-08 GAF  DRILL RERUN PROTECTION PRB5414                 *
      *================================================================*
       01  SDB-H3-REC.
           05  SDBH3-CUST-ID               PIC X(10).
           05  SDBH3-DESC                  PIC X(30).
           05  SDBH3-REF-NO                PIC X(16).
           05  SDBH3-KEY                   PIC X(12).
           05  SDBH3-ADDR                  PIC X(30).
           05  SDBH3-CONTACT               PIC X(20).
           05  SDB-H3-AMOUNTS.
               10  SDBH3-PREPAID-YTD       PIC S9(13)V99 COMP-3.
               10  SDBH3-DRILL-YTD         PIC S9(13)V99 COMP-3.
               10  SDBH3-DEPOSIT-YTD       PIC S9(13)V99 COMP-3.
               10  SDBH3-DRILL-AMT         PIC S9(13)V99 COMP-3.
               10  SDBH3-PREPAID-AMT       PIC S9(13)V99 COMP-3.
               10  SDBH3-RENT-YTD          PIC S9(13)V99 COMP-3.
               10  SDBH3-RENT-AMT          PIC S9(13)V99 COMP-3.
               10  SDBH3-DEPOSIT-AMT       PIC S9(13)V99 COMP-3.
               10  SDBH3-KEY-FEE-AMT       PIC S9(13)V99 COMP-3.
               10  SDBH3-LATE-FEE-AMT      PIC S9(13)V99 COMP-3.
           05  SDB-H3-DATES.
               10  SDBH3-LAST-MAINT-DATE   PIC 9(08).
               10  SDBH3-MATURITY-DATE     PIC 9(08).
               10  SDBH3-LAST-ACT-DATE     PIC 9(08).
               10  SDBH3-CLOSE-DATE        PIC 9(08).
               10  SDBH3-NOTICE-DATE       PIC 9(08).
               10  SDBH3-NEXT-REVIEW-DATE  PIC 9(08).
               10  SDBH3-LAST-STMT-DATE    PIC 9(08).
               10  SDBH3-NEXT-DUE-DATE     PIC 9(08).
           05  SDB-H3-CODES.
               10  SDBH3-REASON-CD         PIC X(01).
               10  SDBH3-PREFERRED-FLAG    PIC X(01).
                   88  SDBH3-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  SDBH3-BRANCH-CD         PIC X(03).
                   88  SDBH3-BRANCH-CD-EXS         VALUE 'EXS'.
                   88  SDBH3-BRANCH-CD-FJW         VALUE 'FJW'.
                   88  SDBH3-BRANCH-CD-YFB         VALUE 'YFB'.
               10  SDBH3-BOX-SIZE-CD       PIC X(02).
                   88  SDBH3-BOX-SIZE-CD-CU        VALUE 'CU'.
                   88  SDBH3-BOX-SIZE-CD-MG        VALUE 'MG'.
                   88  SDBH3-BOX-SIZE-CD-VG        VALUE 'VG'.
                   88  SDBH3-BOX-SIZE-CD-VN        VALUE 'VN'.
                   88  SDBH3-BOX-SIZE-CD-XJ        VALUE 'XJ'.
               10  SDBH3-CYCLE-CD          PIC X(02).
                   88  SDBH3-CYCLE-CD-AP           VALUE 'AP'.
                   88  SDBH3-CYCLE-CD-DA           VALUE 'DA'.
                   88  SDBH3-CYCLE-CD-HV           VALUE 'HV'.
                   88  SDBH3-CYCLE-CD-SE           VALUE 'SE'.
               10  SDBH3-SOURCE-CD         PIC X(02).
                   88  SDBH3-SOURCE-CD-DD          VALUE 'DD'.
                   88  SDBH3-SOURCE-CD-JZ          VALUE 'JZ'.
                   88  SDBH3-SOURCE-CD-PS          VALUE 'PS'.
                   88  SDBH3-SOURCE-CD-UM          VALUE 'UM'.
               10  SDBH3-REVIEW-FLAG       PIC X(01).
                   88  SDBH3-REVIEW-FLAG-YES       VALUE 'Y'.
               10  SDBH3-VAULT-CD          PIC X(02).
                   88  SDBH3-VAULT-CD-BU           VALUE 'BU'.
                   88  SDBH3-VAULT-CD-ET           VALUE 'ET'.
                   88  SDBH3-VAULT-CD-FF           VALUE 'FF'.
                   88  SDBH3-VAULT-CD-NG           VALUE 'NG'.
                   88  SDBH3-VAULT-CD-NH           VALUE 'NH'.
               10  SDBH3-AUTO-RENEW-FLAG   PIC X(01).
                   88  SDBH3-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  SDBH3-EXEMPT-FLAG       PIC X(01).
                   88  SDBH3-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  SDBH3-SUSPEND-FLAG      PIC X(01).
                   88  SDBH3-SUSPEND-FLAG-YES      VALUE 'Y'.
           05  SDB-H3-COUNTERS.
               10  SDBH3-DISCOUNT          PIC 9(03)V9(06).
               10  SDBH3-ITEM-CNT          PIC 9(07).
               10  SDBH3-NOTICE-CNT        PIC 9(07).
               10  SDBH3-PENALTY-PCT       PIC 9(03)V9(06).
               10  SDBH3-FEE-PCT           PIC 9(03)V9(06).
               10  SDBH3-RETRY-CNT         PIC 9(07).
           05  FILLER                      PIC X(20).
