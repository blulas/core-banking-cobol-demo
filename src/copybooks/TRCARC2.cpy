      *================================================================*
      * COPYBOOK : TRCARC2                                             *
      * PURPOSE  : TRAVELERS CHEQUES PRIOR VERSION RECORD LAYOUT       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-06-01 LMB  ORIGINAL                                       *
      * 2001-10-21 PQR  REMIT ROUNDING - HALF UP CHG5358               *
      * 2002-03-01 LMB  FACE FEE SCHEDULE UPDATE CHG7722               *
      * 2008-09-17 CRP  REFUND CALCULATION CORRECTED INC6493           *
      * 2008-01-06 LMB  COMMISSION EXCEPTIONS TO OPS QUEUE CHG7210     *
      * 2012-07-14 EWS  REFUND STATUS CODES ADDED CHG7432              *
      * 2013-10-15 BHS  COMMISSION EXCEPTIONS TO OPS QUEUE INC4143     *
      *================================================================*
       01  TRC-H2-REC.
           05  TRCH2-CUST-ID               PIC X(10).
           05  TRCH2-KEY                   PIC X(12).
           05  TRCH2-DESC                  PIC X(30).
           05  TRCH2-NAME                  PIC X(30).
           05  TRCH2-CONTACT               PIC X(20).
           05  TRCH2-REF-NO                PIC X(16).
           05  TRC-H2-AMOUNTS.
               10  TRCH2-REFUND-YTD        PIC S9(13)V99 COMP-3.
               10  TRCH2-REMIT-YTD         PIC S9(13)V99 COMP-3.
               10  TRCH2-REMIT-AMT         PIC S9(13)V99 COMP-3.
               10  TRCH2-COMMISSION-YTD    PIC S9(13)V99 COMP-3.
               10  TRCH2-FACE-AMT          PIC S9(13)V99 COMP-3.
               10  TRCH2-REFUND-AMT        PIC S9(13)V99 COMP-3.
               10  TRCH2-CONSIGN-YTD       PIC S9(13)V99 COMP-3.
               10  TRCH2-FACE-YTD          PIC S9(13)V99 COMP-3.
               10  TRCH2-SHORTAGE-YTD      PIC S9(13)V99 COMP-3.
               10  TRCH2-SHORTAGE-AMT      PIC S9(13)V99 COMP-3.
               10  TRCH2-CONSIGN-AMT       PIC S9(13)V99 COMP-3.
           05  TRC-H2-DATES.
               10  TRCH2-LAST-STMT-DATE    PIC 9(08).
               10  TRCH2-EFFECTIVE-DATE    PIC 9(08).
               10  TRCH2-LAST-PAY-DATE     PIC 9(08).
               10  TRCH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  TRCH2-MATURITY-DATE     PIC 9(08).
               10  TRCH2-NEXT-DUE-DATE     PIC 9(08).
               10  TRCH2-CLOSE-DATE        PIC 9(08).
               10  TRCH2-LAST-ACT-DATE     PIC 9(08).
               10  TRCH2-EXPIRY-DATE       PIC 9(08).
           05  TRC-H2-CODES.
               10  TRCH2-DENOM-CD          PIC X(01).
                   88  TRCH2-DENOM-CD-G            VALUE 'G'.
                   88  TRCH2-DENOM-CD-M            VALUE 'M'.
                   88  TRCH2-DENOM-CD-N            VALUE 'N'.
                   88  TRCH2-DENOM-CD-P            VALUE 'P'.
                   88  TRCH2-DENOM-CD-V            VALUE 'V'.
                   88  TRCH2-DENOM-CD-Y            VALUE 'Y'.
               10  TRCH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  TRCH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  TRCH2-REVIEW-FLAG       PIC X(01).
                   88  TRCH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  TRCH2-BRANCH-CD         PIC X(02).
                   88  TRCH2-BRANCH-CD-EG          VALUE 'EG'.
                   88  TRCH2-BRANCH-CD-HZ          VALUE 'HZ'.
                   88  TRCH2-BRANCH-CD-JF          VALUE 'JF'.
                   88  TRCH2-BRANCH-CD-NZ          VALUE 'NZ'.
               10  TRCH2-ISSUER-CD         PIC X(02).
                   88  TRCH2-ISSUER-CD-AF          VALUE 'AF'.
                   88  TRCH2-ISSUER-CD-CJ          VALUE 'CJ'.
                   88  TRCH2-ISSUER-CD-DU          VALUE 'DU'.
                   88  TRCH2-ISSUER-CD-JY          VALUE 'JY'.
                   88  TRCH2-ISSUER-CD-RR          VALUE 'RR'.
                   88  TRCH2-ISSUER-CD-TK          VALUE 'TK'.
               10  TRCH2-OVERRIDE-FLAG     PIC X(01).
                   88  TRCH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  TRCH2-CYCLE-CD          PIC X(03).
               10  TRCH2-SOURCE-CD         PIC X(02).
                   88  TRCH2-SOURCE-CD-EG          VALUE 'EG'.
                   88  TRCH2-SOURCE-CD-GH          VALUE 'GH'.
                   88  TRCH2-SOURCE-CD-LC          VALUE 'LC'.
                   88  TRCH2-SOURCE-CD-LU          VALUE 'LU'.
                   88  TRCH2-SOURCE-CD-YS          VALUE 'YS'.
               10  TRCH2-SUSPEND-FLAG      PIC X(01).
                   88  TRCH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  TRCH2-STATUS-CD         PIC X(02).
                   88  TRCH2-STATUS-CD-HK          VALUE 'HK'.
                   88  TRCH2-STATUS-CD-RU          VALUE 'RU'.
                   88  TRCH2-STATUS-CD-WA          VALUE 'WA'.
           05  TRC-H2-COUNTERS.
               10  TRCH2-EXCEPT-CNT        PIC 9(07).
               10  TRCH2-FEE-PCT           PIC 9(03)V9(06).
               10  TRCH2-RETRY-CNT         PIC 9(07).
               10  TRCH2-CYCLE-CNT         PIC 9(07).
               10  TRCH2-MARGIN            PIC 9(03)V9(06).
               10  TRCH2-RATE              PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
