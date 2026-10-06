      *================================================================*
      * COPYBOOK : ATNOLD1                                             *
      * PURPOSE  : ATM NETWORK SETTLEMENT ARCHIVE RECORD LAYOUT        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-06-26 JLM  ORIGINAL                                       *
      * 1995-11-17 DPS  AUDIT FINDING - SURCHG CONTROL TOTALS CHG8269  *
      * 1998-09-19 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2004-10-02 LMB  SETTLE FEE SCHEDULE UPDATE CHG3975             *
      * 2005-07-03 SKN  INTERCHG ROUNDING - HALF UP REQ3721            *
      * 2008-11-13 RJT  DISPENSED NOTICE TEXT PER LEGAL CHG2493        *
      * 2009-04-28 DPS  INTERCHG ROUNDING - HALF UP PRB1997            *
      * 2010-03-04 BHS  AUDIT FINDING - CASSETTE CONTROL TOTALS REQ7383*
      *================================================================*
       01  ATN-H1-REC.
           05  ATNH1-CUST-ID               PIC X(10).
           05  ATNH1-NAME                  PIC X(30).
           05  ATNH1-DESC                  PIC X(30).
           05  ATNH1-ADDR                  PIC X(30).
           05  ATN-H1-AMOUNTS.
               10  ATNH1-SETTLE-AMT        PIC S9(13)V99 COMP-3.
               10  ATNH1-ADJUST-YTD        PIC S9(13)V99 COMP-3.
               10  ATNH1-INTERCHG-YTD      PIC S9(13)V99 COMP-3.
               10  ATNH1-CASSETTE-AMT      PIC S9(13)V99 COMP-3.
               10  ATNH1-DISPENSED-AMT     PIC S9(13)V99 COMP-3.
               10  ATNH1-INTERCHG-AMT      PIC S9(13)V99 COMP-3.
               10  ATNH1-DISPENSED-YTD     PIC S9(13)V99 COMP-3.
               10  ATNH1-SETTLE-YTD        PIC S9(13)V99 COMP-3.
               10  ATNH1-SURCHG-YTD        PIC S9(13)V99 COMP-3.
               10  ATNH1-ADJUST-AMT        PIC S9(13)V99 COMP-3.
           05  ATN-H1-DATES.
               10  ATNH1-OPEN-DATE         PIC 9(08).
               10  ATNH1-CLOSE-DATE        PIC 9(08).
               10  ATNH1-EXPIRY-DATE       PIC 9(08).
               10  ATNH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  ATNH1-NOTICE-DATE       PIC 9(08).
               10  ATNH1-EFFECTIVE-DATE    PIC 9(08).
               10  ATNH1-MATURITY-DATE     PIC 9(08).
               10  ATNH1-NEXT-DUE-DATE     PIC 9(08).
           05  ATN-H1-CODES.
               10  ATNH1-STATUS-CD         PIC X(02).
                   88  ATNH1-STATUS-CD-DF          VALUE 'DF'.
                   88  ATNH1-STATUS-CD-DP          VALUE 'DP'.
                   88  ATNH1-STATUS-CD-FJ          VALUE 'FJ'.
                   88  ATNH1-STATUS-CD-NA          VALUE 'NA'.
               10  ATNH1-BRANCH-CD         PIC X(02).
                   88  ATNH1-BRANCH-CD-DU          VALUE 'DU'.
                   88  ATNH1-BRANCH-CD-HX          VALUE 'HX'.
                   88  ATNH1-BRANCH-CD-YC          VALUE 'YC'.
                   88  ATNH1-BRANCH-CD-ZB          VALUE 'ZB'.
               10  ATNH1-NETWORK-CD        PIC X(01).
               10  ATNH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  ATNH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  ATNH1-EXEMPT-FLAG       PIC X(01).
                   88  ATNH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  ATNH1-SUSPEND-FLAG      PIC X(01).
                   88  ATNH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  ATNH1-HOLD-FLAG         PIC X(01).
                   88  ATNH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  ATNH1-REGION-CD         PIC X(01).
                   88  ATNH1-REGION-CD-F           VALUE 'F'.
                   88  ATNH1-REGION-CD-G           VALUE 'G'.
                   88  ATNH1-REGION-CD-H           VALUE 'H'.
                   88  ATNH1-REGION-CD-P           VALUE 'P'.
                   88  ATNH1-REGION-CD-V           VALUE 'V'.
                   88  ATNH1-REGION-CD-X           VALUE 'X'.
               10  ATNH1-REVIEW-FLAG       PIC X(01).
                   88  ATNH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  ATNH1-STOP-FLAG         PIC X(01).
                   88  ATNH1-STOP-FLAG-YES         VALUE 'Y'.
               10  ATNH1-REASON-CD         PIC X(02).
                   88  ATNH1-REASON-CD-AK          VALUE 'AK'.
                   88  ATNH1-REASON-CD-HX          VALUE 'HX'.
                   88  ATNH1-REASON-CD-PR          VALUE 'PR'.
                   88  ATNH1-REASON-CD-XD          VALUE 'XD'.
                   88  ATNH1-REASON-CD-ZJ          VALUE 'ZJ'.
           05  ATN-H1-COUNTERS.
               10  ATNH1-PAYMENT-CNT       PIC 9(07).
               10  ATNH1-ITEM-CNT          PIC 9(07).
               10  ATNH1-RETRY-CNT         PIC 9(07).
               10  ATNH1-NOTICE-CNT        PIC 9(07).
               10  ATNH1-FEE-PCT           PIC 9(03)V9(06).
               10  ATNH1-RATE              PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
