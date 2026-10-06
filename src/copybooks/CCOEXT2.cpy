      *================================================================*
      * COPYBOOK : CCOEXT2                                             *
      * PURPOSE  : COIN AND CURRENCY EXTRACT RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-04-15 PQR  ORIGINAL                                       *
      * 1987-01-13 LMB  SHIPMENT ROUNDING - HALF UP PRB9879            *
      * 1990-03-26 BHS  COIN NOTICE TEXT PER LEGAL PRB9736             *
      * 1990-01-09 PQR  NEW FEE REPORT COLUMNS REQ7347                 *
      * 1991-05-18 JLM  NEW STRAP REPORT COLUMNS PRB6941               *
      * 1991-01-05 LMB  COIN CALCULATION CORRECTED PRB6405             *
      * 1996-01-01 TWH  SHIPMENT CALCULATION CORRECTED INC8700         *
      * 2001-05-18 LMB  VARIANCE RERUN PROTECTION PRB0191              *
      * 2004-08-14 GAF  CURRENCY EXCEPTIONS TO OPS QUEUE PRB9160       *
      * 2004-11-10 DPS  SHIPMENT ROUNDING - HALF UP CHG5138            *
      *================================================================*
       01  CCO-H2-REC.
           05  CCOH2-DESC                  PIC X(30).
           05  CCOH2-CONTACT               PIC X(20).
           05  CCOH2-REF-NO                PIC X(16).
           05  CCOH2-NAME                  PIC X(30).
           05  CCOH2-ADDR                  PIC X(30).
           05  CCOH2-KEY                   PIC X(12).
           05  CCO-H2-AMOUNTS.
               10  CCOH2-SHIPMENT-YTD      PIC S9(13)V99 COMP-3.
               10  CCOH2-CURRENCY-AMT      PIC S9(13)V99 COMP-3.
               10  CCOH2-COIN-YTD          PIC S9(13)V99 COMP-3.
               10  CCOH2-COIN-AMT          PIC S9(13)V99 COMP-3.
               10  CCOH2-VARIANCE-AMT      PIC S9(13)V99 COMP-3.
               10  CCOH2-VARIANCE-YTD      PIC S9(13)V99 COMP-3.
               10  CCOH2-FEE-YTD           PIC S9(13)V99 COMP-3.
               10  CCOH2-STRAP-AMT         PIC S9(13)V99 COMP-3.
               10  CCOH2-FEE-AMT           PIC S9(13)V99 COMP-3.
               10  CCOH2-SHIPMENT-AMT      PIC S9(13)V99 COMP-3.
           05  CCO-H2-DATES.
               10  CCOH2-MATURITY-DATE     PIC 9(08).
               10  CCOH2-NEXT-DUE-DATE     PIC 9(08).
               10  CCOH2-NOTICE-DATE       PIC 9(08).
               10  CCOH2-OPEN-DATE         PIC 9(08).
               10  CCOH2-LAST-ACT-DATE     PIC 9(08).
               10  CCOH2-EXPIRY-DATE       PIC 9(08).
               10  CCOH2-EFFECTIVE-DATE    PIC 9(08).
               10  CCOH2-LAST-PAY-DATE     PIC 9(08).
               10  CCOH2-NEXT-REVIEW-DATE  PIC 9(08).
           05  CCO-H2-CODES.
               10  CCOH2-OVERRIDE-FLAG     PIC X(01).
                   88  CCOH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  CCOH2-CHANNEL-CD        PIC X(02).
                   88  CCOH2-CHANNEL-CD-AW         VALUE 'AW'.
                   88  CCOH2-CHANNEL-CD-DW         VALUE 'DW'.
                   88  CCOH2-CHANNEL-CD-GN         VALUE 'GN'.
                   88  CCOH2-CHANNEL-CD-RS         VALUE 'RS'.
                   88  CCOH2-CHANNEL-CD-UZ         VALUE 'UZ'.
                   88  CCOH2-CHANNEL-CD-YR         VALUE 'YR'.
               10  CCOH2-EXEMPT-FLAG       PIC X(01).
                   88  CCOH2-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  CCOH2-AUTO-RENEW-FLAG   PIC X(01).
                   88  CCOH2-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  CCOH2-SOURCE-CD         PIC X(03).
               10  CCOH2-STOP-FLAG         PIC X(01).
                   88  CCOH2-STOP-FLAG-YES         VALUE 'Y'.
               10  CCOH2-CARRIER-CD        PIC X(01).
                   88  CCOH2-CARRIER-CD-J          VALUE 'J'.
                   88  CCOH2-CARRIER-CD-L          VALUE 'L'.
                   88  CCOH2-CARRIER-CD-M          VALUE 'M'.
                   88  CCOH2-CARRIER-CD-X          VALUE 'X'.
               10  CCOH2-HOLD-FLAG         PIC X(01).
                   88  CCOH2-HOLD-FLAG-YES         VALUE 'Y'.
               10  CCOH2-REASON-CD         PIC X(02).
                   88  CCOH2-REASON-CD-JL          VALUE 'JL'.
                   88  CCOH2-REASON-CD-LW          VALUE 'LW'.
                   88  CCOH2-REASON-CD-PS          VALUE 'PS'.
               10  CCOH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  CCOH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  CCOH2-STATUS-CD         PIC X(03).
           05  CCO-H2-COUNTERS.
               10  CCOH2-ITEM-CNT          PIC 9(07).
               10  CCOH2-CYCLE-CNT         PIC 9(07).
               10  CCOH2-FEE-PCT           PIC 9(03)V9(06).
               10  CCOH2-RETRY-CNT         PIC 9(07).
               10  CCOH2-NOTICE-CNT        PIC 9(07).
               10  CCOH2-PENALTY-PCT       PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
