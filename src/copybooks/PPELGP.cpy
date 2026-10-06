      *================================================================*
      * COPYBOOK : PPELGP                                              *
      * PURPOSE  : LINKAGE FOR POSITIVE PAY RULE MODULE PPELG          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-28 CRP  ORIGINAL                                       *
      * 1992-06-28 SKN  VOID RERUN PROTECTION INC9804                  *
      * 1993-08-22 MKL  ISSUED CALCULATION CORRECTED INC9076           *
      * 1995-11-09 TWH  STALE FEE SCHEDULE UPDATE REQ5330              *
      * 1996-02-15 PQR  EXCEPT TABLE EXPANDED INC3677                  *
      * 1999-02-14 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-05-02 MKL  PERFORMANCE - ISSUED READ AHEAD PRB6057        *
      * 2003-06-28 GAF  EXCEPT ROUNDING - HALF UP PRB8050              *
      *================================================================*
       01  PPELG-PARMS.
           05  PPELG-PAID-AMT              PIC S9(13)V99 COMP-3.
           05  PPELG-PAID-YTD              PIC S9(13)V99 COMP-3.
           05  PPELG-VOID-AMT              PIC S9(13)V99 COMP-3.
           05  PPELG-ISSUED-YTD            PIC S9(13)V99 COMP-3.
           05  PPELG-EXCEPT-AMT            PIC S9(13)V99 COMP-3.
           05  PPELG-STALE-AMT             PIC S9(13)V99 COMP-3.
           05  PPELG-NEXT-DUE-DATE         PIC 9(08).
           05  PPELG-EFFECTIVE-DATE        PIC 9(08).
           05  PPELG-EXPIRY-DATE           PIC 9(08).
           05  PPELG-CLOSE-DATE            PIC 9(08).
           05  PPELG-STATUS-CD             PIC X(02).
               88  PPELG-STATUS-CD-DR              VALUE 'DR'.
               88  PPELG-STATUS-CD-TK              VALUE 'TK'.
               88  PPELG-STATUS-CD-WX              VALUE 'WX'.
           05  PPELG-REASON-CD             PIC X(01).
               88  PPELG-REASON-CD-B               VALUE 'B'.
               88  PPELG-REASON-CD-M               VALUE 'M'.
               88  PPELG-REASON-CD-N               VALUE 'N'.
               88  PPELG-REASON-CD-U               VALUE 'U'.
               88  PPELG-REASON-CD-V               VALUE 'V'.
               88  PPELG-REASON-CD-Z               VALUE 'Z'.
           05  PPELG-CYCLE-CD              PIC X(01).
               88  PPELG-CYCLE-CD-A                VALUE 'A'.
               88  PPELG-CYCLE-CD-B                VALUE 'B'.
               88  PPELG-CYCLE-CD-C                VALUE 'C'.
               88  PPELG-CYCLE-CD-R                VALUE 'R'.
               88  PPELG-CYCLE-CD-T                VALUE 'T'.
               88  PPELG-CYCLE-CD-Y                VALUE 'Y'.
           05  PPELG-REGION-CD             PIC X(01).
               88  PPELG-REGION-CD-A               VALUE 'A'.
               88  PPELG-REGION-CD-B               VALUE 'B'.
               88  PPELG-REGION-CD-D               VALUE 'D'.
               88  PPELG-REGION-CD-H               VALUE 'H'.
               88  PPELG-REGION-CD-N               VALUE 'N'.
               88  PPELG-REGION-CD-Y               VALUE 'Y'.
           05  PPELG-HOLD-FLAG             PIC X(01).
               88  PPELG-HOLD-FLAG-YES             VALUE 'Y'.
           05  PPELG-STOP-FLAG             PIC X(01).
               88  PPELG-STOP-FLAG-YES             VALUE 'Y'.
           05  PPELG-NOTICE-SENT-FLAG      PIC X(01).
               88  PPELG-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  PPELG-EXCEPT-CNT            PIC 9(07).
           05  PPELG-CYCLE-CNT             PIC 9(07).
           05  PPELG-DISCOUNT              PIC 9(03)V9(06).
           05  PPELG-MARGIN                PIC 9(03)V9(06).
           05  PPELG-REF-NO                PIC X(16).
           05  PPELG-CONTACT               PIC X(20).
           05  PPELG-RETURN-CODE           PIC 9(02).
