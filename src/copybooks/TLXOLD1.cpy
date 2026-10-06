      *================================================================*
      * COPYBOOK : TLXOLD1                                             *
      * PURPOSE  : TELEX WIRE MESSAGING EXTRACT RECORD LAYOUT          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-06-24 RJT  ORIGINAL                                       *
      * 1993-03-10 TWH  CORRESP NOTICE TEXT PER LEGAL INC8426          *
      * 1994-12-21 LMB  QUEUED STATUS CODES ADDED INC8600              *
      * 2002-09-19 SKN  SETTLE ROUNDING - HALF UP INC9896              *
      * 2002-01-02 SKN  COVER LIMIT REVISED CHG4932                    *
      * 2006-12-15 BHS  COVER CALCULATION CORRECTED PRB7470            *
      * 2010-01-10 LMB  QUEUED STATUS CODES ADDED REQ8664              *
      *================================================================*
       01  TLX-H1-REC.
           05  TLXH1-REF-NO                PIC X(16).
           05  TLXH1-ADDR                  PIC X(30).
           05  TLXH1-CUST-ID               PIC X(10).
           05  TLXH1-NAME                  PIC X(30).
           05  TLXH1-KEY                   PIC X(12).
           05  TLXH1-DESC                  PIC X(30).
           05  TLXH1-CONTACT               PIC X(20).
           05  TLX-H1-AMOUNTS.
               10  TLXH1-SETTLE-AMT        PIC S9(13)V99 COMP-3.
               10  TLXH1-CORRESP-AMT       PIC S9(13)V99 COMP-3.
               10  TLXH1-SETTLE-YTD        PIC S9(13)V99 COMP-3.
               10  TLXH1-CHARGE-YTD        PIC S9(13)V99 COMP-3.
               10  TLXH1-COVER-YTD         PIC S9(13)V99 COMP-3.
               10  TLXH1-QUEUED-YTD        PIC S9(13)V99 COMP-3.
               10  TLXH1-WIRE-YTD          PIC S9(13)V99 COMP-3.
               10  TLXH1-CHARGE-AMT        PIC S9(13)V99 COMP-3.
               10  TLXH1-WIRE-AMT          PIC S9(13)V99 COMP-3.
               10  TLXH1-COVER-AMT         PIC S9(13)V99 COMP-3.
               10  TLXH1-QUEUED-AMT        PIC S9(13)V99 COMP-3.
           05  TLX-H1-DATES.
               10  TLXH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  TLXH1-EXPIRY-DATE       PIC 9(08).
               10  TLXH1-OPEN-DATE         PIC 9(08).
               10  TLXH1-CLOSE-DATE        PIC 9(08).
               10  TLXH1-NOTICE-DATE       PIC 9(08).
               10  TLXH1-EFFECTIVE-DATE    PIC 9(08).
               10  TLXH1-LAST-PAY-DATE     PIC 9(08).
               10  TLXH1-LAST-ACT-DATE     PIC 9(08).
           05  TLX-H1-CODES.
               10  TLXH1-TEST-KEY-CD       PIC X(02).
               10  TLXH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  TLXH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  TLXH1-REVIEW-FLAG       PIC X(01).
                   88  TLXH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  TLXH1-REGION-CD         PIC X(01).
                   88  TLXH1-REGION-CD-G           VALUE 'G'.
                   88  TLXH1-REGION-CD-M           VALUE 'M'.
                   88  TLXH1-REGION-CD-T           VALUE 'T'.
               10  TLXH1-OVERRIDE-FLAG     PIC X(01).
                   88  TLXH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  TLXH1-PREFERRED-FLAG    PIC X(01).
                   88  TLXH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  TLXH1-MSG-TYPE-CD       PIC X(02).
                   88  TLXH1-MSG-TYPE-CD-LG        VALUE 'LG'.
                   88  TLXH1-MSG-TYPE-CD-LX        VALUE 'LX'.
                   88  TLXH1-MSG-TYPE-CD-ND        VALUE 'ND'.
                   88  TLXH1-MSG-TYPE-CD-PT        VALUE 'PT'.
               10  TLXH1-STOP-FLAG         PIC X(01).
                   88  TLXH1-STOP-FLAG-YES         VALUE 'Y'.
               10  TLXH1-BRANCH-CD         PIC X(02).
                   88  TLXH1-BRANCH-CD-CC          VALUE 'CC'.
                   88  TLXH1-BRANCH-CD-KR          VALUE 'KR'.
                   88  TLXH1-BRANCH-CD-LD          VALUE 'LD'.
                   88  TLXH1-BRANCH-CD-LS          VALUE 'LS'.
                   88  TLXH1-BRANCH-CD-XL          VALUE 'XL'.
               10  TLXH1-TYPE-CD           PIC X(01).
                   88  TLXH1-TYPE-CD-K             VALUE 'K'.
                   88  TLXH1-TYPE-CD-L             VALUE 'L'.
                   88  TLXH1-TYPE-CD-M             VALUE 'M'.
                   88  TLXH1-TYPE-CD-P             VALUE 'P'.
                   88  TLXH1-TYPE-CD-W             VALUE 'W'.
                   88  TLXH1-TYPE-CD-Z             VALUE 'Z'.
               10  TLXH1-SUSPEND-FLAG      PIC X(01).
                   88  TLXH1-SUSPEND-FLAG-YES      VALUE 'Y'.
           05  TLX-H1-COUNTERS.
               10  TLXH1-EXCEPT-CNT        PIC 9(07).
               10  TLXH1-ITEM-CNT          PIC 9(07).
               10  TLXH1-DISCOUNT          PIC 9(03)V9(06).
               10  TLXH1-PAYMENT-CNT       PIC 9(07).
               10  TLXH1-RATE              PIC 9(03)V9(06).
               10  TLXH1-NOTICE-CNT        PIC 9(07).
               10  TLXH1-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
