      *================================================================*
      * COPYBOOK : COLARC2                                             *
      * PURPOSE  : COLLECTIONS QUEUE HISTORY RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-09-07 RJT  ORIGINAL                                       *
      * 1996-01-08 CRP  COLLECTED RERUN PROTECTION INC5545             *
      * 2000-03-06 MKL  AUDIT FINDING - COLLECTED CONTROL TOTALS PRB459*
      * 2000-08-10 RJT  FEE CALCULATION CORRECTED CHG5957              *
      * 2000-02-02 GAF  PROMISED CALCULATION CORRECTED INC9835         *
      * 2003-02-18 DPS  FEE LIMIT REVISED PRB6748                      *
      * 2003-09-12 CRP  PERFORMANCE - PROMISED READ AHEAD CHG0856      *
      *================================================================*
       01  COL-H2-REC.
           05  COLH2-CUST-ID               PIC X(10).
           05  COLH2-CONTACT               PIC X(20).
           05  COLH2-REF-NO                PIC X(16).
           05  COLH2-NAME                  PIC X(30).
           05  COLH2-ADDR                  PIC X(30).
           05  COLH2-KEY                   PIC X(12).
           05  COL-H2-AMOUNTS.
               10  COLH2-PAST-DUE-AMT      PIC S9(13)V99 COMP-3.
               10  COLH2-FEE-AMT           PIC S9(13)V99 COMP-3.
               10  COLH2-COLLECTED-YTD     PIC S9(13)V99 COMP-3.
               10  COLH2-WRITEOFF-YTD      PIC S9(13)V99 COMP-3.
               10  COLH2-WRITEOFF-AMT      PIC S9(13)V99 COMP-3.
               10  COLH2-PAST-DUE-YTD      PIC S9(13)V99 COMP-3.
               10  COLH2-COLLECTED-AMT     PIC S9(13)V99 COMP-3.
               10  COLH2-PROMISED-AMT      PIC S9(13)V99 COMP-3.
               10  COLH2-PROMISED-YTD      PIC S9(13)V99 COMP-3.
               10  COLH2-FEE-YTD           PIC S9(13)V99 COMP-3.
           05  COL-H2-DATES.
               10  COLH2-EXPIRY-DATE       PIC 9(08).
               10  COLH2-NEXT-DUE-DATE     PIC 9(08).
               10  COLH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  COLH2-LAST-MAINT-DATE   PIC 9(08).
               10  COLH2-NOTICE-DATE       PIC 9(08).
               10  COLH2-EFFECTIVE-DATE    PIC 9(08).
               10  COLH2-OPEN-DATE         PIC 9(08).
           05  COL-H2-CODES.
               10  COLH2-ACTION-CD         PIC X(02).
                   88  COLH2-ACTION-CD-KJ          VALUE 'KJ'.
                   88  COLH2-ACTION-CD-TM          VALUE 'TM'.
                   88  COLH2-ACTION-CD-VL          VALUE 'VL'.
                   88  COLH2-ACTION-CD-ZS          VALUE 'ZS'.
               10  COLH2-EXEMPT-FLAG       PIC X(01).
                   88  COLH2-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  COLH2-REVIEW-FLAG       PIC X(01).
                   88  COLH2-REVIEW-FLAG-YES       VALUE 'Y'.
               10  COLH2-REGION-CD         PIC X(02).
                   88  COLH2-REGION-CD-AT          VALUE 'AT'.
                   88  COLH2-REGION-CD-AU          VALUE 'AU'.
                   88  COLH2-REGION-CD-XR          VALUE 'XR'.
                   88  COLH2-REGION-CD-XU          VALUE 'XU'.
               10  COLH2-REASON-CD         PIC X(02).
                   88  COLH2-REASON-CD-AT          VALUE 'AT'.
                   88  COLH2-REASON-CD-BJ          VALUE 'BJ'.
                   88  COLH2-REASON-CD-GG          VALUE 'GG'.
                   88  COLH2-REASON-CD-ZR          VALUE 'ZR'.
               10  COLH2-SUSPEND-FLAG      PIC X(01).
                   88  COLH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  COLH2-OVERRIDE-FLAG     PIC X(01).
                   88  COLH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  COLH2-PREFERRED-FLAG    PIC X(01).
                   88  COLH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  COLH2-STATUS-CD         PIC X(02).
                   88  COLH2-STATUS-CD-PS          VALUE 'PS'.
                   88  COLH2-STATUS-CD-UR          VALUE 'UR'.
                   88  COLH2-STATUS-CD-XM          VALUE 'XM'.
                   88  COLH2-STATUS-CD-YA          VALUE 'YA'.
           05  COL-H2-COUNTERS.
               10  COLH2-ITEM-CNT          PIC 9(07).
               10  COLH2-PENALTY-PCT       PIC 9(03)V9(06).
               10  COLH2-NOTICE-CNT        PIC 9(07).
               10  COLH2-CYCLE-CNT         PIC 9(07).
               10  COLH2-RETRY-CNT         PIC 9(07).
               10  COLH2-MARGIN            PIC 9(03)V9(06).
               10  COLH2-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
