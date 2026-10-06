      *================================================================*
      * COPYBOOK : SDLIMP                                              *
      * PURPOSE  : LINKAGE FOR SAFE DEPOSIT BOXES RULE MODULE SDLIM    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1993-03-04 RJT  ORIGINAL                                       *
      * 1997-11-26 BHS  LATE-FEE EXCEPTIONS TO OPS QUEUE CHG0189       *
      * 2003-09-10 DPS  KEY-FEE LIMIT REVISED CHG4814                  *
      * 2003-01-08 JLM  PREPAID ROUNDING - HALF UP PRB8261             *
      * 2003-03-26 RJT  PERFORMANCE - KEY-FEE READ AHEAD PRB8556       *
      * 2005-08-27 GAF  DEPOSIT LIMIT REVISED CHG5918                  *
      * 2006-12-16 RJT  PERFORMANCE - PREPAID READ AHEAD REQ4275       *
      * 2006-12-16 PQR  PREPAID LIMIT REVISED CHG4497                  *
      *================================================================*
       01  SDLIM-PARMS.
           05  SDLIM-RENT-YTD              PIC S9(13)V99 COMP-3.
           05  SDLIM-DEPOSIT-YTD           PIC S9(13)V99 COMP-3.
           05  SDLIM-LATE-FEE-YTD          PIC S9(13)V99 COMP-3.
           05  SDLIM-DEPOSIT-AMT           PIC S9(13)V99 COMP-3.
           05  SDLIM-LATE-FEE-AMT          PIC S9(13)V99 COMP-3.
           05  SDLIM-DRILL-YTD             PIC S9(13)V99 COMP-3.
           05  SDLIM-LAST-ACT-DATE         PIC 9(08).
           05  SDLIM-OPEN-DATE             PIC 9(08).
           05  SDLIM-NEXT-DUE-DATE         PIC 9(08).
           05  SDLIM-NOTICE-DATE           PIC 9(08).
           05  SDLIM-CYCLE-CD              PIC X(02).
               88  SDLIM-CYCLE-CD-AP               VALUE 'AP'.
               88  SDLIM-CYCLE-CD-DA               VALUE 'DA'.
               88  SDLIM-CYCLE-CD-HV               VALUE 'HV'.
               88  SDLIM-CYCLE-CD-SE               VALUE 'SE'.
           05  SDLIM-BRANCH-CD             PIC X(03).
               88  SDLIM-BRANCH-CD-EXS             VALUE 'EXS'.
               88  SDLIM-BRANCH-CD-FJW             VALUE 'FJW'.
               88  SDLIM-BRANCH-CD-YFB             VALUE 'YFB'.
           05  SDLIM-REASON-CD             PIC X(01).
           05  SDLIM-BOX-SIZE-CD           PIC X(02).
               88  SDLIM-BOX-SIZE-CD-CU            VALUE 'CU'.
               88  SDLIM-BOX-SIZE-CD-MG            VALUE 'MG'.
               88  SDLIM-BOX-SIZE-CD-VG            VALUE 'VG'.
               88  SDLIM-BOX-SIZE-CD-VN            VALUE 'VN'.
               88  SDLIM-BOX-SIZE-CD-XJ            VALUE 'XJ'.
           05  SDLIM-OVERRIDE-FLAG         PIC X(01).
               88  SDLIM-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  SDLIM-EXEMPT-FLAG           PIC X(01).
               88  SDLIM-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  SDLIM-REVIEW-FLAG           PIC X(01).
               88  SDLIM-REVIEW-FLAG-YES           VALUE 'Y'.
           05  SDLIM-RETRY-CNT             PIC 9(07).
           05  SDLIM-NOTICE-CNT            PIC 9(07).
           05  SDLIM-DISCOUNT              PIC 9(03)V9(06).
           05  SDLIM-FEE-PCT               PIC 9(03)V9(06).
           05  SDLIM-CONTACT               PIC X(20).
           05  SDLIM-ADDR                  PIC X(30).
           05  SDLIM-RETURN-CODE           PIC 9(02).
