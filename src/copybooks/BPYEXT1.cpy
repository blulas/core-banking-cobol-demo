      *================================================================*
      * COPYBOOK : BPYEXT1                                             *
      * PURPOSE  : BILL PAYMENT SERVICE ARCHIVE RECORD LAYOUT          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1992-12-23 BHS  ORIGINAL                                       *
      * 1995-04-22 LMB  RETURNED RERUN PROTECTION INC4192              *
      * 1995-03-23 DPS  REFUND FEE SCHEDULE UPDATE REQ7326             *
      * 1997-06-01 EWS  PAYMENT RERUN PROTECTION PRB3523               *
      * 2001-11-19 GAF  REFUND TABLE EXPANDED PRB4523                  *
      * 2002-09-14 JLM  REFUND CALCULATION CORRECTED CHG2278           *
      * 2002-01-04 GAF  PAYMENT CUTOFF MOVED PRB0539                   *
      * 2003-06-12 CRP  PAYMENT RERUN PROTECTION REQ8644               *
      * 2006-05-24 DPS  RETURNED CALCULATION CORRECTED REQ8841         *
      * 2007-12-03 BHS  PAYMENT EXCEPTIONS TO OPS QUEUE INC2023        *
      *================================================================*
       01  BPY-H1-REC.
           05  BPYH1-CONTACT               PIC X(20).
           05  BPYH1-CUST-ID               PIC X(10).
           05  BPYH1-DESC                  PIC X(30).
           05  BPYH1-REF-NO                PIC X(16).
           05  BPYH1-ADDR                  PIC X(30).
           05  BPYH1-NAME                  PIC X(30).
           05  BPYH1-KEY                   PIC X(12).
           05  BPY-H1-AMOUNTS.
               10  BPYH1-RETURNED-AMT      PIC S9(13)V99 COMP-3.
               10  BPYH1-RETURNED-YTD      PIC S9(13)V99 COMP-3.
               10  BPYH1-FEE-YTD           PIC S9(13)V99 COMP-3.
               10  BPYH1-PAYMENT-YTD       PIC S9(13)V99 COMP-3.
               10  BPYH1-FEE-AMT           PIC S9(13)V99 COMP-3.
               10  BPYH1-PAYMENT-AMT       PIC S9(13)V99 COMP-3.
               10  BPYH1-REFUND-AMT        PIC S9(13)V99 COMP-3.
           05  BPY-H1-DATES.
               10  BPYH1-MATURITY-DATE     PIC 9(08).
               10  BPYH1-NOTICE-DATE       PIC 9(08).
               10  BPYH1-CLOSE-DATE        PIC 9(08).
               10  BPYH1-LAST-STMT-DATE    PIC 9(08).
               10  BPYH1-EXPIRY-DATE       PIC 9(08).
               10  BPYH1-EFFECTIVE-DATE    PIC 9(08).
               10  BPYH1-LAST-ACT-DATE     PIC 9(08).
               10  BPYH1-OPEN-DATE         PIC 9(08).
               10  BPYH1-NEXT-DUE-DATE     PIC 9(08).
           05  BPY-H1-CODES.
               10  BPYH1-HOLD-FLAG         PIC X(01).
                   88  BPYH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  BPYH1-SOURCE-CD         PIC X(02).
               10  BPYH1-CYCLE-CD          PIC X(02).
               10  BPYH1-PAYEE-TYPE-CD     PIC X(02).
                   88  BPYH1-PAYEE-TYPE-CD-BT      VALUE 'BT'.
                   88  BPYH1-PAYEE-TYPE-CD-FR      VALUE 'FR'.
                   88  BPYH1-PAYEE-TYPE-CD-LL      VALUE 'LL'.
                   88  BPYH1-PAYEE-TYPE-CD-US      VALUE 'US'.
                   88  BPYH1-PAYEE-TYPE-CD-XM      VALUE 'XM'.
                   88  BPYH1-PAYEE-TYPE-CD-YE      VALUE 'YE'.
               10  BPYH1-REMIT-METHOD-CD   PIC X(03).
               10  BPYH1-REASON-CD         PIC X(02).
               10  BPYH1-PREFERRED-FLAG    PIC X(01).
                   88  BPYH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  BPYH1-STATUS-CD         PIC X(02).
                   88  BPYH1-STATUS-CD-JX          VALUE 'JX'.
                   88  BPYH1-STATUS-CD-LR          VALUE 'LR'.
                   88  BPYH1-STATUS-CD-NZ          VALUE 'NZ'.
                   88  BPYH1-STATUS-CD-RV          VALUE 'RV'.
                   88  BPYH1-STATUS-CD-TM          VALUE 'TM'.
                   88  BPYH1-STATUS-CD-TZ          VALUE 'TZ'.
               10  BPYH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  BPYH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  BPYH1-OVERRIDE-FLAG     PIC X(01).
                   88  BPYH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  BPYH1-SUSPEND-FLAG      PIC X(01).
                   88  BPYH1-SUSPEND-FLAG-YES      VALUE 'Y'.
           05  BPY-H1-COUNTERS.
               10  BPYH1-MARGIN            PIC 9(03)V9(06).
               10  BPYH1-ITEM-CNT          PIC 9(07).
               10  BPYH1-RATE              PIC 9(03)V9(06).
               10  BPYH1-CYCLE-CNT         PIC 9(07).
               10  BPYH1-PAYMENT-CNT       PIC 9(07).
               10  BPYH1-EXCEPT-CNT        PIC 9(07).
               10  BPYH1-PENALTY-PCT       PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
