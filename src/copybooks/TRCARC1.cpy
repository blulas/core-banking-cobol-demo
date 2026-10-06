      *================================================================*
      * COPYBOOK : TRCARC1                                             *
      * PURPOSE  : TRAVELERS CHEQUES PRIOR VERSION RECORD LAYOUT       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-09-05 JLM  ORIGINAL                                       *
      * 1991-04-06 EWS  PERFORMANCE - REFUND READ AHEAD CHG4634        *
      * 1991-05-08 PQR  AUDIT FINDING - REFUND CONTROL TOTALS PRB6457  *
      * 1993-03-13 EWS  COMMISSION ROUNDING - HALF UP CHG2036          *
      * 1994-11-14 JLM  SHORTAGE NOTICE TEXT PER LEGAL REQ1785         *
      * 2000-11-02 LMB  SHORTAGE RERUN PROTECTION REQ9649              *
      * 2004-02-08 GAF  NEW CONSIGN REPORT COLUMNS INC1865             *
      * 2005-04-19 LMB  AUDIT FINDING - COMMISSION CONTROL TOTALS INC20*
      * 2013-12-08 TWH  REMIT EXCEPTIONS TO OPS QUEUE PRB9231          *
      *================================================================*
       01  TRC-H1-REC.
           05  TRCH1-CUST-ID               PIC X(10).
           05  TRCH1-DESC                  PIC X(30).
           05  TRCH1-ADDR                  PIC X(30).
           05  TRCH1-REF-NO                PIC X(16).
           05  TRCH1-CONTACT               PIC X(20).
           05  TRCH1-KEY                   PIC X(12).
           05  TRC-H1-AMOUNTS.
               10  TRCH1-FACE-YTD          PIC S9(13)V99 COMP-3.
               10  TRCH1-COMMISSION-YTD    PIC S9(13)V99 COMP-3.
               10  TRCH1-REFUND-YTD        PIC S9(13)V99 COMP-3.
               10  TRCH1-SHORTAGE-YTD      PIC S9(13)V99 COMP-3.
               10  TRCH1-REMIT-AMT         PIC S9(13)V99 COMP-3.
               10  TRCH1-COMMISSION-AMT    PIC S9(13)V99 COMP-3.
               10  TRCH1-SHORTAGE-AMT      PIC S9(13)V99 COMP-3.
               10  TRCH1-REFUND-AMT        PIC S9(13)V99 COMP-3.
               10  TRCH1-REMIT-YTD         PIC S9(13)V99 COMP-3.
               10  TRCH1-CONSIGN-AMT       PIC S9(13)V99 COMP-3.
               10  TRCH1-FACE-AMT          PIC S9(13)V99 COMP-3.
               10  TRCH1-CONSIGN-YTD       PIC S9(13)V99 COMP-3.
           05  TRC-H1-DATES.
               10  TRCH1-LAST-STMT-DATE    PIC 9(08).
               10  TRCH1-CLOSE-DATE        PIC 9(08).
               10  TRCH1-MATURITY-DATE     PIC 9(08).
               10  TRCH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  TRCH1-EXPIRY-DATE       PIC 9(08).
               10  TRCH1-EFFECTIVE-DATE    PIC 9(08).
               10  TRCH1-LAST-PAY-DATE     PIC 9(08).
               10  TRCH1-NEXT-DUE-DATE     PIC 9(08).
               10  TRCH1-LAST-ACT-DATE     PIC 9(08).
           05  TRC-H1-CODES.
               10  TRCH1-DENOM-CD          PIC X(01).
                   88  TRCH1-DENOM-CD-G            VALUE 'G'.
                   88  TRCH1-DENOM-CD-M            VALUE 'M'.
                   88  TRCH1-DENOM-CD-N            VALUE 'N'.
                   88  TRCH1-DENOM-CD-P            VALUE 'P'.
                   88  TRCH1-DENOM-CD-V            VALUE 'V'.
                   88  TRCH1-DENOM-CD-Y            VALUE 'Y'.
               10  TRCH1-SOURCE-CD         PIC X(02).
                   88  TRCH1-SOURCE-CD-EG          VALUE 'EG'.
                   88  TRCH1-SOURCE-CD-GH          VALUE 'GH'.
                   88  TRCH1-SOURCE-CD-LC          VALUE 'LC'.
                   88  TRCH1-SOURCE-CD-LU          VALUE 'LU'.
                   88  TRCH1-SOURCE-CD-YS          VALUE 'YS'.
               10  TRCH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  TRCH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  TRCH1-BRANCH-CD         PIC X(02).
                   88  TRCH1-BRANCH-CD-EG          VALUE 'EG'.
                   88  TRCH1-BRANCH-CD-HZ          VALUE 'HZ'.
                   88  TRCH1-BRANCH-CD-JF          VALUE 'JF'.
                   88  TRCH1-BRANCH-CD-NZ          VALUE 'NZ'.
               10  TRCH1-REVIEW-FLAG       PIC X(01).
                   88  TRCH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  TRCH1-CYCLE-CD          PIC X(03).
               10  TRCH1-STOP-FLAG         PIC X(01).
                   88  TRCH1-STOP-FLAG-YES         VALUE 'Y'.
               10  TRCH1-EXEMPT-FLAG       PIC X(01).
                   88  TRCH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  TRCH1-OVERRIDE-FLAG     PIC X(01).
                   88  TRCH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  TRCH1-ISSUER-CD         PIC X(02).
                   88  TRCH1-ISSUER-CD-AF          VALUE 'AF'.
                   88  TRCH1-ISSUER-CD-CJ          VALUE 'CJ'.
                   88  TRCH1-ISSUER-CD-DU          VALUE 'DU'.
                   88  TRCH1-ISSUER-CD-JY          VALUE 'JY'.
                   88  TRCH1-ISSUER-CD-RR          VALUE 'RR'.
                   88  TRCH1-ISSUER-CD-TK          VALUE 'TK'.
           05  TRC-H1-COUNTERS.
               10  TRCH1-EXCEPT-CNT        PIC 9(07).
               10  TRCH1-RETRY-CNT         PIC 9(07).
               10  TRCH1-MARGIN            PIC 9(03)V9(06).
               10  TRCH1-ITEM-CNT          PIC 9(07).
               10  TRCH1-FEE-PCT           PIC 9(03)V9(06).
               10  TRCH1-RATE              PIC 9(03)V9(06).
               10  TRCH1-CYCLE-CNT         PIC 9(07).
           05  FILLER                      PIC X(20).
