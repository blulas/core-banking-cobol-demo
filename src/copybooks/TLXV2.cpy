      *================================================================*
      * COPYBOOK : TLXV2                                               *
      * PURPOSE  : TELEX WIRE MESSAGING ARCHIVE RECORD LAYOUT          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-02-26 BHS  ORIGINAL                                       *
      * 1995-05-17 RJT  QUEUED RERUN PROTECTION CHG7604                *
      * 1999-05-19 CRP  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-04-20 RJT  WIRE FEE SCHEDULE UPDATE INC7897               *
      * 2009-05-03 RJT  CORRESP EXCEPTIONS TO OPS QUEUE REQ7328        *
      *================================================================*
       01  TLX-H2-REC.
           05  TLXH2-NAME                  PIC X(30).
           05  TLXH2-ADDR                  PIC X(30).
           05  TLXH2-DESC                  PIC X(30).
           05  TLXH2-REF-NO                PIC X(16).
           05  TLXH2-KEY                   PIC X(12).
           05  TLXH2-CONTACT               PIC X(20).
           05  TLX-H2-AMOUNTS.
               10  TLXH2-QUEUED-YTD        PIC S9(13)V99 COMP-3.
               10  TLXH2-WIRE-AMT          PIC S9(13)V99 COMP-3.
               10  TLXH2-QUEUED-AMT        PIC S9(13)V99 COMP-3.
               10  TLXH2-SETTLE-AMT        PIC S9(13)V99 COMP-3.
               10  TLXH2-CORRESP-AMT       PIC S9(13)V99 COMP-3.
               10  TLXH2-SETTLE-YTD        PIC S9(13)V99 COMP-3.
               10  TLXH2-WIRE-YTD          PIC S9(13)V99 COMP-3.
               10  TLXH2-CHARGE-AMT        PIC S9(13)V99 COMP-3.
               10  TLXH2-COVER-AMT         PIC S9(13)V99 COMP-3.
               10  TLXH2-CHARGE-YTD        PIC S9(13)V99 COMP-3.
               10  TLXH2-COVER-YTD         PIC S9(13)V99 COMP-3.
           05  TLX-H2-DATES.
               10  TLXH2-OPEN-DATE         PIC 9(08).
               10  TLXH2-LAST-ACT-DATE     PIC 9(08).
               10  TLXH2-NEXT-DUE-DATE     PIC 9(08).
               10  TLXH2-EFFECTIVE-DATE    PIC 9(08).
               10  TLXH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  TLXH2-NOTICE-DATE       PIC 9(08).
               10  TLXH2-EXPIRY-DATE       PIC 9(08).
               10  TLXH2-LAST-PAY-DATE     PIC 9(08).
               10  TLXH2-CLOSE-DATE        PIC 9(08).
           05  TLX-H2-CODES.
               10  TLXH2-OVERRIDE-FLAG     PIC X(01).
                   88  TLXH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  TLXH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  TLXH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  TLXH2-REVIEW-FLAG       PIC X(01).
                   88  TLXH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  TLXH2-REGION-CD         PIC X(01).
                   88  TLXH2-REGION-CD-G           VALUE 'G'.
                   88  TLXH2-REGION-CD-M           VALUE 'M'.
                   88  TLXH2-REGION-CD-T           VALUE 'T'.
               10  TLXH2-BRANCH-CD         PIC X(02).
                   88  TLXH2-BRANCH-CD-CC          VALUE 'CC'.
                   88  TLXH2-BRANCH-CD-KR          VALUE 'KR'.
                   88  TLXH2-BRANCH-CD-LD          VALUE 'LD'.
                   88  TLXH2-BRANCH-CD-LS          VALUE 'LS'.
                   88  TLXH2-BRANCH-CD-XL          VALUE 'XL'.
               10  TLXH2-TYPE-CD           PIC X(01).
                   88  TLXH2-TYPE-CD-K             VALUE 'K'.
                   88  TLXH2-TYPE-CD-L             VALUE 'L'.
                   88  TLXH2-TYPE-CD-M             VALUE 'M'.
                   88  TLXH2-TYPE-CD-P             VALUE 'P'.
                   88  TLXH2-TYPE-CD-W             VALUE 'W'.
                   88  TLXH2-TYPE-CD-Z             VALUE 'Z'.
               10  TLXH2-MSG-TYPE-CD       PIC X(02).
                   88  TLXH2-MSG-TYPE-CD-LG        VALUE 'LG'.
                   88  TLXH2-MSG-TYPE-CD-LX        VALUE 'LX'.
                   88  TLXH2-MSG-TYPE-CD-ND        VALUE 'ND'.
                   88  TLXH2-MSG-TYPE-CD-PT        VALUE 'PT'.
               10  TLXH2-REASON-CD         PIC X(03).
               10  TLXH2-SUSPEND-FLAG      PIC X(01).
                   88  TLXH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  TLXH2-STOP-FLAG         PIC X(01).
                   88  TLXH2-STOP-FLAG-YES         VALUE 'Y'.
               10  TLXH2-TEST-KEY-CD       PIC X(02).
               10  TLXH2-PREFERRED-FLAG    PIC X(01).
                   88  TLXH2-PREFERRED-FLAG-YES    VALUE 'Y'.
           05  TLX-H2-COUNTERS.
               10  TLXH2-ITEM-CNT          PIC 9(07).
               10  TLXH2-RATE              PIC 9(03)V9(06).
               10  TLXH2-EXCEPT-CNT        PIC 9(07).
               10  TLXH2-FEE-PCT           PIC 9(03)V9(06).
               10  TLXH2-PAYMENT-CNT       PIC 9(07).
               10  TLXH2-DISCOUNT          PIC 9(03)V9(06).
               10  TLXH2-NOTICE-CNT        PIC 9(07).
           05  FILLER                      PIC X(20).
