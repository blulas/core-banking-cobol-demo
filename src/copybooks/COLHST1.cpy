      *================================================================*
      * COPYBOOK : COLHST1                                             *
      * PURPOSE  : COLLECTIONS QUEUE HISTORY RECORD LAYOUT             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-02-11 GAF  ORIGINAL                                       *
      * 1998-04-20 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-12-20 PQR  PAST-DUE TABLE EXPANDED PRB8705                *
      * 2001-07-07 JLM  WRITEOFF CUTOFF MOVED CHG7496                  *
      * 2002-08-10 LMB  WRITEOFF ROUNDING - HALF UP PRB0903            *
      * 2003-05-23 BHS  PERFORMANCE - PAST-DUE READ AHEAD PRB6457      *
      * 2003-04-26 CRP  PROMISED FEE SCHEDULE UPDATE CHG7087           *
      * 2004-04-15 SKN  COLLECTED CUTOFF MOVED REQ2324                 *
      * 2004-08-27 GAF  PERFORMANCE - PAST-DUE READ AHEAD CHG4334      *
      * 2004-08-15 RJT  PAST-DUE CUTOFF MOVED PRB0474                  *
      *================================================================*
       01  COL-H1-REC.
           05  COLH1-CUST-ID               PIC X(10).
           05  COLH1-KEY                   PIC X(12).
           05  COLH1-ADDR                  PIC X(30).
           05  COLH1-CONTACT               PIC X(20).
           05  COLH1-NAME                  PIC X(30).
           05  COLH1-DESC                  PIC X(30).
           05  COLH1-REF-NO                PIC X(16).
           05  COL-H1-AMOUNTS.
               10  COLH1-COLLECTED-YTD     PIC S9(13)V99 COMP-3.
               10  COLH1-PROMISED-YTD      PIC S9(13)V99 COMP-3.
               10  COLH1-PAST-DUE-AMT      PIC S9(13)V99 COMP-3.
               10  COLH1-PROMISED-AMT      PIC S9(13)V99 COMP-3.
               10  COLH1-WRITEOFF-YTD      PIC S9(13)V99 COMP-3.
               10  COLH1-FEE-YTD           PIC S9(13)V99 COMP-3.
               10  COLH1-COLLECTED-AMT     PIC S9(13)V99 COMP-3.
               10  COLH1-PAST-DUE-YTD      PIC S9(13)V99 COMP-3.
               10  COLH1-WRITEOFF-AMT      PIC S9(13)V99 COMP-3.
               10  COLH1-FEE-AMT           PIC S9(13)V99 COMP-3.
           05  COL-H1-DATES.
               10  COLH1-OPEN-DATE         PIC 9(08).
               10  COLH1-LAST-MAINT-DATE   PIC 9(08).
               10  COLH1-NEXT-DUE-DATE     PIC 9(08).
               10  COLH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  COLH1-CLOSE-DATE        PIC 9(08).
               10  COLH1-LAST-PAY-DATE     PIC 9(08).
           05  COL-H1-CODES.
               10  COLH1-CYCLE-CD          PIC X(02).
               10  COLH1-EXEMPT-FLAG       PIC X(01).
                   88  COLH1-EXEMPT-FLAG-YES       VALUE 'Y'.
               10  COLH1-QUEUE-CD          PIC X(02).
               10  COLH1-REVIEW-FLAG       PIC X(01).
                   88  COLH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  COLH1-REASON-CD         PIC X(02).
                   88  COLH1-REASON-CD-AT          VALUE 'AT'.
                   88  COLH1-REASON-CD-BJ          VALUE 'BJ'.
                   88  COLH1-REASON-CD-GG          VALUE 'GG'.
                   88  COLH1-REASON-CD-ZR          VALUE 'ZR'.
               10  COLH1-SUSPEND-FLAG      PIC X(01).
                   88  COLH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  COLH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  COLH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  COLH1-REGION-CD         PIC X(02).
                   88  COLH1-REGION-CD-AT          VALUE 'AT'.
                   88  COLH1-REGION-CD-AU          VALUE 'AU'.
                   88  COLH1-REGION-CD-XR          VALUE 'XR'.
                   88  COLH1-REGION-CD-XU          VALUE 'XU'.
               10  COLH1-PREFERRED-FLAG    PIC X(01).
                   88  COLH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  COLH1-STATUS-CD         PIC X(02).
                   88  COLH1-STATUS-CD-PS          VALUE 'PS'.
                   88  COLH1-STATUS-CD-UR          VALUE 'UR'.
                   88  COLH1-STATUS-CD-XM          VALUE 'XM'.
                   88  COLH1-STATUS-CD-YA          VALUE 'YA'.
               10  COLH1-OVERRIDE-FLAG     PIC X(01).
                   88  COLH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  COLH1-ACTION-CD         PIC X(02).
                   88  COLH1-ACTION-CD-KJ          VALUE 'KJ'.
                   88  COLH1-ACTION-CD-TM          VALUE 'TM'.
                   88  COLH1-ACTION-CD-VL          VALUE 'VL'.
                   88  COLH1-ACTION-CD-ZS          VALUE 'ZS'.
           05  COL-H1-COUNTERS.
               10  COLH1-NOTICE-CNT        PIC 9(07).
               10  COLH1-CYCLE-CNT         PIC 9(07).
               10  COLH1-PENALTY-PCT       PIC 9(03)V9(06).
               10  COLH1-ITEM-CNT          PIC 9(07).
               10  COLH1-RETRY-CNT         PIC 9(07).
               10  COLH1-MARGIN            PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
