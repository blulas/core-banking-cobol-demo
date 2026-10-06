      *================================================================*
      * COPYBOOK : CCOV1                                               *
      * PURPOSE  : COIN AND CURRENCY PRIOR VERSION RECORD LAYOUT       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-02-07 EWS  ORIGINAL                                       *
      * 1989-12-14 PQR  COIN CALCULATION CORRECTED CHG5777             *
      * 1989-01-07 CRP  SHIPMENT ROUNDING - HALF UP CHG5795            *
      * 1990-03-13 TWH  PERFORMANCE - FEE READ AHEAD INC8180           *
      * 1993-08-15 GAF  CURRENCY FEE SCHEDULE UPDATE REQ9794           *
      * 1994-02-10 PQR  SHIPMENT EXCEPTIONS TO OPS QUEUE CHG1573       *
      * 1994-01-07 MKL  AUDIT FINDING - COIN CONTROL TOTALS REQ7050    *
      * 1999-10-13 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-06-28 PQR  SHIPMENT RERUN PROTECTION CHG8864              *
      * 2000-06-14 JLM  VARIANCE CALCULATION CORRECTED INC1986         *
      * 2002-12-22 EWS  CURRENCY NOTICE TEXT PER LEGAL PRB2448         *
      * 2003-09-26 DPS  SHIPMENT LIMIT REVISED CHG7689                 *
      *================================================================*
       01  CCO-H1-REC.
           05  CCOH1-DESC                  PIC X(30).
           05  CCOH1-KEY                   PIC X(12).
           05  CCOH1-NAME                  PIC X(30).
           05  CCOH1-CUST-ID               PIC X(10).
           05  CCOH1-REF-NO                PIC X(16).
           05  CCOH1-ADDR                  PIC X(30).
           05  CCOH1-CONTACT               PIC X(20).
           05  CCO-H1-AMOUNTS.
               10  CCOH1-STRAP-YTD         PIC S9(13)V99 COMP-3.
               10  CCOH1-COIN-YTD          PIC S9(13)V99 COMP-3.
               10  CCOH1-STRAP-AMT         PIC S9(13)V99 COMP-3.
               10  CCOH1-FEE-YTD           PIC S9(13)V99 COMP-3.
               10  CCOH1-VARIANCE-AMT      PIC S9(13)V99 COMP-3.
               10  CCOH1-SHIPMENT-YTD      PIC S9(13)V99 COMP-3.
               10  CCOH1-COIN-AMT          PIC S9(13)V99 COMP-3.
               10  CCOH1-CURRENCY-AMT      PIC S9(13)V99 COMP-3.
               10  CCOH1-CURRENCY-YTD      PIC S9(13)V99 COMP-3.
               10  CCOH1-VARIANCE-YTD      PIC S9(13)V99 COMP-3.
               10  CCOH1-SHIPMENT-AMT      PIC S9(13)V99 COMP-3.
               10  CCOH1-FEE-AMT           PIC S9(13)V99 COMP-3.
           05  CCO-H1-DATES.
               10  CCOH1-OPEN-DATE         PIC 9(08).
               10  CCOH1-MATURITY-DATE     PIC 9(08).
               10  CCOH1-EFFECTIVE-DATE    PIC 9(08).
               10  CCOH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  CCOH1-LAST-PAY-DATE     PIC 9(08).
               10  CCOH1-EXPIRY-DATE       PIC 9(08).
               10  CCOH1-NEXT-DUE-DATE     PIC 9(08).
               10  CCOH1-LAST-ACT-DATE     PIC 9(08).
               10  CCOH1-NOTICE-DATE       PIC 9(08).
           05  CCO-H1-CODES.
               10  CCOH1-STOP-FLAG         PIC X(01).
                   88  CCOH1-STOP-FLAG-YES         VALUE 'Y'.
               10  CCOH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  CCOH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  CCOH1-CHANNEL-CD        PIC X(02).
                   88  CCOH1-CHANNEL-CD-AW         VALUE 'AW'.
                   88  CCOH1-CHANNEL-CD-DW         VALUE 'DW'.
                   88  CCOH1-CHANNEL-CD-GN         VALUE 'GN'.
                   88  CCOH1-CHANNEL-CD-RS         VALUE 'RS'.
                   88  CCOH1-CHANNEL-CD-UZ         VALUE 'UZ'.
                   88  CCOH1-CHANNEL-CD-YR         VALUE 'YR'.
               10  CCOH1-SOURCE-CD         PIC X(03).
               10  CCOH1-REASON-CD         PIC X(02).
                   88  CCOH1-REASON-CD-JL          VALUE 'JL'.
                   88  CCOH1-REASON-CD-LW          VALUE 'LW'.
                   88  CCOH1-REASON-CD-PS          VALUE 'PS'.
               10  CCOH1-STATUS-CD         PIC X(03).
               10  CCOH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  CCOH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  CCOH1-EXEMPT-FLAG       PIC X(01).
                   88  CCOH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  CCOH1-DENOM-CD          PIC X(01).
                   88  CCOH1-DENOM-CD-B            VALUE 'B'.
                   88  CCOH1-DENOM-CD-D            VALUE 'D'.
                   88  CCOH1-DENOM-CD-P            VALUE 'P'.
                   88  CCOH1-DENOM-CD-S            VALUE 'S'.
                   88  CCOH1-DENOM-CD-W            VALUE 'W'.
               10  CCOH1-OVERRIDE-FLAG     PIC X(01).
                   88  CCOH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  CCOH1-HOLD-FLAG         PIC X(01).
                   88  CCOH1-HOLD-FLAG-YES         VALUE 'Y'.
           05  CCO-H1-COUNTERS.
               10  CCOH1-CYCLE-CNT         PIC 9(07).
               10  CCOH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  CCOH1-RETRY-CNT         PIC 9(07).
               10  CCOH1-FEE-PCT           PIC 9(03)V9(06).
               10  CCOH1-ITEM-CNT          PIC 9(07).
               10  CCOH1-RATE              PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
