      *================================================================*
      * COPYBOOK : TRCV3                                               *
      * PURPOSE  : TRAVELERS CHEQUES HISTORY RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-12-17 DPS  ORIGINAL                                       *
      * 1992-06-15 BHS  NEW REMIT REPORT COLUMNS INC5651               *
      * 1995-12-02 SKN  SHORTAGE NOTICE TEXT PER LEGAL PRB8231         *
      * 1996-11-11 JLM  REFUND LIMIT REVISED INC4604                   *
      * 1997-01-11 EWS  COMMISSION CUTOFF MOVED REQ8503                *
      * 1999-05-19 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2003-08-01 SKN  COMMISSION LIMIT REVISED REQ1768               *
      * 2005-10-23 LMB  CONSIGN EXCEPTIONS TO OPS QUEUE REQ3960        *
      * 2009-01-18 BHS  FACE ROUNDING - HALF UP INC2183                *
      * 2010-02-02 MKL  FACE CALCULATION CORRECTED PRB3334             *
      * 2011-10-22 SKN  REMIT LIMIT REVISED INC2407                    *
      * 2014-12-06 MKL  AUDIT FINDING - REFUND CONTROL TOTALS REQ7494  *
      *================================================================*
       01  TRC-H3-REC.
           05  TRCH3-CUST-ID               PIC X(10).
           05  TRCH3-DESC                  PIC X(30).
           05  TRCH3-CONTACT               PIC X(20).
           05  TRCH3-KEY                   PIC X(12).
           05  TRCH3-REF-NO                PIC X(16).
           05  TRC-H3-AMOUNTS.
               10  TRCH3-COMMISSION-AMT    PIC S9(13)V99 COMP-3.
               10  TRCH3-CONSIGN-AMT       PIC S9(13)V99 COMP-3.
               10  TRCH3-REMIT-AMT         PIC S9(13)V99 COMP-3.
               10  TRCH3-REFUND-AMT        PIC S9(13)V99 COMP-3.
               10  TRCH3-REFUND-YTD        PIC S9(13)V99 COMP-3.
               10  TRCH3-REMIT-YTD         PIC S9(13)V99 COMP-3.
               10  TRCH3-CONSIGN-YTD       PIC S9(13)V99 COMP-3.
               10  TRCH3-SHORTAGE-YTD      PIC S9(13)V99 COMP-3.
               10  TRCH3-SHORTAGE-AMT      PIC S9(13)V99 COMP-3.
               10  TRCH3-FACE-YTD          PIC S9(13)V99 COMP-3.
               10  TRCH3-COMMISSION-YTD    PIC S9(13)V99 COMP-3.
               10  TRCH3-FACE-AMT          PIC S9(13)V99 COMP-3.
           05  TRC-H3-DATES.
               10  TRCH3-EFFECTIVE-DATE    PIC 9(08).
               10  TRCH3-LAST-STMT-DATE    PIC 9(08).
               10  TRCH3-NEXT-REVIEW-DATE  PIC 9(08).
               10  TRCH3-MATURITY-DATE     PIC 9(08).
               10  TRCH3-EXPIRY-DATE       PIC 9(08).
               10  TRCH3-NEXT-DUE-DATE     PIC 9(08).
               10  TRCH3-LAST-PAY-DATE     PIC 9(08).
               10  TRCH3-CLOSE-DATE        PIC 9(08).
           05  TRC-H3-CODES.
               10  TRCH3-REVIEW-FLAG       PIC X(01).
                   88  TRCH3-REVIEW-FLAG-YES       VALUE 'Y'.
               10  TRCH3-SUSPEND-FLAG      PIC X(01).
                   88  TRCH3-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  TRCH3-NOTICE-SENT-FLAG  PIC X(01).
                   88  TRCH3-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  TRCH3-STATUS-CD         PIC X(02).
                   88  TRCH3-STATUS-CD-HK          VALUE 'HK'.
                   88  TRCH3-STATUS-CD-RU          VALUE 'RU'.
                   88  TRCH3-STATUS-CD-WA          VALUE 'WA'.
               10  TRCH3-SOURCE-CD         PIC X(02).
                   88  TRCH3-SOURCE-CD-EG          VALUE 'EG'.
                   88  TRCH3-SOURCE-CD-GH          VALUE 'GH'.
                   88  TRCH3-SOURCE-CD-LC          VALUE 'LC'.
                   88  TRCH3-SOURCE-CD-LU          VALUE 'LU'.
                   88  TRCH3-SOURCE-CD-YS          VALUE 'YS'.
               10  TRCH3-CYCLE-CD          PIC X(03).
               10  TRCH3-BRANCH-CD         PIC X(02).
                   88  TRCH3-BRANCH-CD-EG          VALUE 'EG'.
                   88  TRCH3-BRANCH-CD-HZ          VALUE 'HZ'.
                   88  TRCH3-BRANCH-CD-JF          VALUE 'JF'.
                   88  TRCH3-BRANCH-CD-NZ          VALUE 'NZ'.
               10  TRCH3-STOP-FLAG         PIC X(01).
                   88  TRCH3-STOP-FLAG-YES         VALUE 'Y'.
               10  TRCH3-DENOM-CD          PIC X(01).
                   88  TRCH3-DENOM-CD-G            VALUE 'G'.
                   88  TRCH3-DENOM-CD-M            VALUE 'M'.
                   88  TRCH3-DENOM-CD-N            VALUE 'N'.
                   88  TRCH3-DENOM-CD-P            VALUE 'P'.
                   88  TRCH3-DENOM-CD-V            VALUE 'V'.
                   88  TRCH3-DENOM-CD-Y            VALUE 'Y'.
               10  TRCH3-ISSUER-CD         PIC X(02).
                   88  TRCH3-ISSUER-CD-AF          VALUE 'AF'.
                   88  TRCH3-ISSUER-CD-CJ          VALUE 'CJ'.
                   88  TRCH3-ISSUER-CD-DU          VALUE 'DU'.
                   88  TRCH3-ISSUER-CD-JY          VALUE 'JY'.
                   88  TRCH3-ISSUER-CD-RR          VALUE 'RR'.
                   88  TRCH3-ISSUER-CD-TK          VALUE 'TK'.
           05  TRC-H3-COUNTERS.
               10  TRCH3-ITEM-CNT          PIC 9(07).
               10  TRCH3-EXCEPT-CNT        PIC 9(07).
               10  TRCH3-FEE-PCT           PIC 9(03)V9(06).
               10  TRCH3-RATE              PIC 9(03)V9(06).
               10  TRCH3-MARGIN            PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
