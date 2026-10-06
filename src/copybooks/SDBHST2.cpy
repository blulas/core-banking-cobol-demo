      *================================================================*
      * COPYBOOK : SDBHST2                                             *
      * PURPOSE  : SAFE DEPOSIT BOXES EXTRACT RECORD LAYOUT            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-06-20 DPS  ORIGINAL                                       *
      * 1996-09-23 EWS  RENT EXCEPTIONS TO OPS QUEUE CHG2023           *
      * 1997-03-28 JLM  DRILL CALCULATION CORRECTED INC5458            *
      * 1999-02-07 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-05-17 PQR  NEW LATE-FEE REPORT COLUMNS INC6748            *
      * 2000-11-21 EWS  PREPAID TABLE EXPANDED INC6281                 *
      * 2001-06-26 BHS  LATE-FEE RERUN PROTECTION PRB2034              *
      * 2001-06-13 BHS  NEW PREPAID REPORT COLUMNS INC6169             *
      * 2007-08-25 GAF  RENT LIMIT REVISED CHG4975                     *
      * 2008-06-10 SKN  RENT LIMIT REVISED PRB4065                     *
      * 2008-06-10 TWH  PREPAID RERUN PROTECTION REQ4564               *
      * 2008-10-15 LMB  LATE-FEE RERUN PROTECTION INC7844              *
      *================================================================*
       01  SDB-H2-REC.
           05  SDBH2-ADDR                  PIC X(30).
           05  SDBH2-CUST-ID               PIC X(10).
           05  SDBH2-CONTACT               PIC X(20).
           05  SDBH2-NAME                  PIC X(30).
           05  SDBH2-REF-NO                PIC X(16).
           05  SDBH2-DESC                  PIC X(30).
           05  SDB-H2-AMOUNTS.
               10  SDBH2-LATE-FEE-YTD      PIC S9(13)V99 COMP-3.
               10  SDBH2-RENT-YTD          PIC S9(13)V99 COMP-3.
               10  SDBH2-KEY-FEE-AMT       PIC S9(13)V99 COMP-3.
               10  SDBH2-KEY-FEE-YTD       PIC S9(13)V99 COMP-3.
               10  SDBH2-PREPAID-YTD       PIC S9(13)V99 COMP-3.
               10  SDBH2-DRILL-YTD         PIC S9(13)V99 COMP-3.
               10  SDBH2-RENT-AMT          PIC S9(13)V99 COMP-3.
               10  SDBH2-PREPAID-AMT       PIC S9(13)V99 COMP-3.
               10  SDBH2-DRILL-AMT         PIC S9(13)V99 COMP-3.
               10  SDBH2-DEPOSIT-YTD       PIC S9(13)V99 COMP-3.
               10  SDBH2-LATE-FEE-AMT      PIC S9(13)V99 COMP-3.
           05  SDB-H2-DATES.
               10  SDBH2-NOTICE-DATE       PIC 9(08).
               10  SDBH2-MATURITY-DATE     PIC 9(08).
               10  SDBH2-LAST-ACT-DATE     PIC 9(08).
               10  SDBH2-LAST-MAINT-DATE   PIC 9(08).
               10  SDBH2-LAST-STMT-DATE    PIC 9(08).
               10  SDBH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  SDBH2-CLOSE-DATE        PIC 9(08).
               10  SDBH2-NEXT-DUE-DATE     PIC 9(08).
           05  SDB-H2-CODES.
               10  SDBH2-CYCLE-CD          PIC X(02).
                   88  SDBH2-CYCLE-CD-AP           VALUE 'AP'.
                   88  SDBH2-CYCLE-CD-DA           VALUE 'DA'.
                   88  SDBH2-CYCLE-CD-HV           VALUE 'HV'.
                   88  SDBH2-CYCLE-CD-SE           VALUE 'SE'.
               10  SDBH2-SUSPEND-FLAG      PIC X(01).
                   88  SDBH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  SDBH2-OVERRIDE-FLAG     PIC X(01).
                   88  SDBH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  SDBH2-REASON-CD         PIC X(01).
               10  SDBH2-SOURCE-CD         PIC X(02).
                   88  SDBH2-SOURCE-CD-DD          VALUE 'DD'.
                   88  SDBH2-SOURCE-CD-JZ          VALUE 'JZ'.
                   88  SDBH2-SOURCE-CD-PS          VALUE 'PS'.
                   88  SDBH2-SOURCE-CD-UM          VALUE 'UM'.
               10  SDBH2-VAULT-CD          PIC X(02).
                   88  SDBH2-VAULT-CD-BU           VALUE 'BU'.
                   88  SDBH2-VAULT-CD-ET           VALUE 'ET'.
                   88  SDBH2-VAULT-CD-FF           VALUE 'FF'.
                   88  SDBH2-VAULT-CD-NG           VALUE 'NG'.
                   88  SDBH2-VAULT-CD-NH           VALUE 'NH'.
               10  SDBH2-BRANCH-CD         PIC X(03).
                   88  SDBH2-BRANCH-CD-EXS         VALUE 'EXS'.
                   88  SDBH2-BRANCH-CD-FJW         VALUE 'FJW'.
                   88  SDBH2-BRANCH-CD-YFB         VALUE 'YFB'.
               10  SDBH2-REVIEW-FLAG       PIC X(01).
                   88  SDBH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  SDBH2-PREFERRED-FLAG    PIC X(01).
                   88  SDBH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  SDBH2-EXEMPT-FLAG       PIC X(01).
                   88  SDBH2-EXEMPT-FLAG-YES       VALUE 'Y'.
           05  SDB-H2-COUNTERS.
               10  SDBH2-FEE-PCT           PIC 9(03)V9(06).
               10  SDBH2-NOTICE-CNT        PIC 9(07).
               10  SDBH2-PAYMENT-CNT       PIC 9(07).
               10  SDBH2-PENALTY-PCT       PIC 9(03)V9(06).
               10  SDBH2-ITEM-CNT          PIC 9(07).
               10  SDBH2-DISCOUNT          PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
