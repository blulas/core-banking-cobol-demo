      *================================================================*
      * COPYBOOK : SDVALP                                              *
      * PURPOSE  : LINKAGE FOR SAFE DEPOSIT BOXES RULE MODULE SDVAL    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-02-25 GAF  ORIGINAL                                       *
      * 1994-11-20 BHS  LATE-FEE STATUS CODES ADDED INC1214            *
      * 1995-12-26 SKN  DRILL STATUS CODES ADDED CHG2710               *
      * 1998-08-24 BHS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-12-08 EWS  LATE-FEE FEE SCHEDULE UPDATE PRB6131           *
      * 2000-07-16 RJT  LATE-FEE ROUNDING - HALF UP PRB7693            *
      * 2004-09-27 EWS  DEPOSIT CUTOFF MOVED INC5766                   *
      * 2007-10-02 MKL  PREPAID LIMIT REVISED CHG5262                  *
      *================================================================*
       01  SDVAL-PARMS.
           05  SDVAL-DRILL-YTD             PIC S9(13)V99 COMP-3.
           05  SDVAL-KEY-FEE-YTD           PIC S9(13)V99 COMP-3.
           05  SDVAL-DEPOSIT-AMT           PIC S9(13)V99 COMP-3.
           05  SDVAL-RENT-AMT              PIC S9(13)V99 COMP-3.
           05  SDVAL-RENT-YTD              PIC S9(13)V99 COMP-3.
           05  SDVAL-DRILL-AMT             PIC S9(13)V99 COMP-3.
           05  SDVAL-NEXT-DUE-DATE         PIC 9(08).
           05  SDVAL-LAST-MAINT-DATE       PIC 9(08).
           05  SDVAL-OPEN-DATE             PIC 9(08).
           05  SDVAL-LAST-STMT-DATE        PIC 9(08).
           05  SDVAL-CYCLE-CD              PIC X(02).
               88  SDVAL-CYCLE-CD-AP               VALUE 'AP'.
               88  SDVAL-CYCLE-CD-DA               VALUE 'DA'.
               88  SDVAL-CYCLE-CD-HV               VALUE 'HV'.
               88  SDVAL-CYCLE-CD-SE               VALUE 'SE'.
           05  SDVAL-BOX-SIZE-CD           PIC X(02).
               88  SDVAL-BOX-SIZE-CD-CU            VALUE 'CU'.
               88  SDVAL-BOX-SIZE-CD-MG            VALUE 'MG'.
               88  SDVAL-BOX-SIZE-CD-VG            VALUE 'VG'.
               88  SDVAL-BOX-SIZE-CD-VN            VALUE 'VN'.
               88  SDVAL-BOX-SIZE-CD-XJ            VALUE 'XJ'.
           05  SDVAL-REASON-CD             PIC X(01).
           05  SDVAL-BRANCH-CD             PIC X(03).
               88  SDVAL-BRANCH-CD-EXS             VALUE 'EXS'.
               88  SDVAL-BRANCH-CD-FJW             VALUE 'FJW'.
               88  SDVAL-BRANCH-CD-YFB             VALUE 'YFB'.
           05  SDVAL-OVERRIDE-FLAG         PIC X(01).
               88  SDVAL-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  SDVAL-AUTO-RENEW-FLAG       PIC X(01).
               88  SDVAL-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  SDVAL-REVIEW-FLAG           PIC X(01).
               88  SDVAL-REVIEW-FLAG-YES           VALUE 'Y'.
           05  SDVAL-NOTICE-CNT            PIC 9(07).
           05  SDVAL-RETRY-CNT             PIC 9(07).
           05  SDVAL-PENALTY-PCT           PIC 9(03)V9(06).
           05  SDVAL-DISCOUNT              PIC 9(03)V9(06).
           05  SDVAL-NAME                  PIC X(30).
           05  SDVAL-CONTACT               PIC X(20).
           05  SDVAL-RETURN-CODE           PIC 9(02).
