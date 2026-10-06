      *================================================================*
      * COPYBOOK : IRAV1                                               *
      * PURPOSE  : IRA CUSTODY HISTORY RECORD LAYOUT                   *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1995-05-04 EWS  ORIGINAL                                       *
      * 1996-02-01 LMB  AUDIT FINDING - DISTRIB CONTROL TOTALS CHG6315 *
      * 1999-06-13 PQR  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-09-08 PQR  CONTRIB FEE SCHEDULE UPDATE INC2304            *
      * 2002-05-19 MKL  WITHHOLD ROUNDING - HALF UP INC9235            *
      * 2003-06-11 SKN  DISTRIB CALCULATION CORRECTED PRB0479          *
      * 2008-04-27 JLM  WITHHOLD LIMIT REVISED REQ4324                 *
      * 2008-01-18 LMB  WITHHOLD FEE SCHEDULE UPDATE INC3291           *
      * 2010-01-08 BHS  WITHHOLD FEE SCHEDULE UPDATE REQ9089           *
      *================================================================*
       01  IRA-H1-REC.
           05  IRAH1-KEY                   PIC X(12).
           05  IRAH1-NAME                  PIC X(30).
           05  IRAH1-CUST-ID               PIC X(10).
           05  IRAH1-DESC                  PIC X(30).
           05  IRAH1-CONTACT               PIC X(20).
           05  IRA-H1-AMOUNTS.
               10  IRAH1-RMD-AMT           PIC S9(13)V99 COMP-3.
               10  IRAH1-ROLLOVER-AMT      PIC S9(13)V99 COMP-3.
               10  IRAH1-FMV-AMT           PIC S9(13)V99 COMP-3.
               10  IRAH1-ROLLOVER-YTD      PIC S9(13)V99 COMP-3.
               10  IRAH1-WITHHOLD-AMT      PIC S9(13)V99 COMP-3.
               10  IRAH1-CONTRIB-YTD       PIC S9(13)V99 COMP-3.
               10  IRAH1-EXCESS-YTD        PIC S9(13)V99 COMP-3.
               10  IRAH1-WITHHOLD-YTD      PIC S9(13)V99 COMP-3.
               10  IRAH1-FMV-YTD           PIC S9(13)V99 COMP-3.
               10  IRAH1-CONTRIB-AMT       PIC S9(13)V99 COMP-3.
               10  IRAH1-DISTRIB-AMT       PIC S9(13)V99 COMP-3.
               10  IRAH1-RMD-YTD           PIC S9(13)V99 COMP-3.
               10  IRAH1-DISTRIB-YTD       PIC S9(13)V99 COMP-3.
               10  IRAH1-EXCESS-AMT        PIC S9(13)V99 COMP-3.
           05  IRA-H1-DATES.
               10  IRAH1-EFFECTIVE-DATE    PIC 9(08).
               10  IRAH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  IRAH1-NEXT-DUE-DATE     PIC 9(08).
               10  IRAH1-NOTICE-DATE       PIC 9(08).
               10  IRAH1-LAST-STMT-DATE    PIC 9(08).
               10  IRAH1-LAST-ACT-DATE     PIC 9(08).
               10  IRAH1-EXPIRY-DATE       PIC 9(08).
               10  IRAH1-OPEN-DATE         PIC 9(08).
           05  IRA-H1-CODES.
               10  IRAH1-IRA-TYPE-CD       PIC X(01).
                   88  IRAH1-IRA-TYPE-CD-F         VALUE 'F'.
                   88  IRAH1-IRA-TYPE-CD-V         VALUE 'V'.
                   88  IRAH1-IRA-TYPE-CD-W         VALUE 'W'.
                   88  IRAH1-IRA-TYPE-CD-Y         VALUE 'Y'.
               10  IRAH1-TYPE-CD           PIC X(02).
                   88  IRAH1-TYPE-CD-EA            VALUE 'EA'.
                   88  IRAH1-TYPE-CD-HR            VALUE 'HR'.
                   88  IRAH1-TYPE-CD-XZ            VALUE 'XZ'.
               10  IRAH1-REGION-CD         PIC X(02).
               10  IRAH1-HOLD-FLAG         PIC X(01).
                   88  IRAH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  IRAH1-AUTO-RENEW-FLAG   PIC X(01).
                   88  IRAH1-AUTO-RENEW-FLAG-YES   VALUE 'Y'.
               10  IRAH1-OVERRIDE-FLAG     PIC X(01).
                   88  IRAH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  IRAH1-DIST-CODE-CD      PIC X(01).
                   88  IRAH1-DIST-CODE-CD-C        VALUE 'C'.
                   88  IRAH1-DIST-CODE-CD-J        VALUE 'J'.
                   88  IRAH1-DIST-CODE-CD-U        VALUE 'U'.
                   88  IRAH1-DIST-CODE-CD-X        VALUE 'X'.
               10  IRAH1-REASON-CD         PIC X(03).
                   88  IRAH1-REASON-CD-CMR         VALUE 'CMR'.
                   88  IRAH1-REASON-CD-LJS         VALUE 'LJS'.
                   88  IRAH1-REASON-CD-NRT         VALUE 'NRT'.
                   88  IRAH1-REASON-CD-UGA         VALUE 'UGA'.
                   88  IRAH1-REASON-CD-WBS         VALUE 'WBS'.
                   88  IRAH1-REASON-CD-ZYU         VALUE 'ZYU'.
               10  IRAH1-REVIEW-FLAG       PIC X(01).
                   88  IRAH1-REVIEW-FLAG-YES       VALUE 'Y'.
               10  IRAH1-SUSPEND-FLAG      PIC X(01).
                   88  IRAH1-SUSPEND-FLAG-YES      VALUE 'Y'.
           05  IRA-H1-COUNTERS.
               10  IRAH1-ITEM-CNT          PIC 9(07).
               10  IRAH1-MARGIN            PIC 9(03)V9(06).
               10  IRAH1-PAYMENT-CNT       PIC 9(07).
               10  IRAH1-RATE              PIC 9(03)V9(06).
               10  IRAH1-RETRY-CNT         PIC 9(07).
               10  IRAH1-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
