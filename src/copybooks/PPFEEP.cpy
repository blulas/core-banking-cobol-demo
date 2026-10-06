      *================================================================*
      * COPYBOOK : PPFEEP                                              *
      * PURPOSE  : LINKAGE FOR POSITIVE PAY RULE MODULE PPFEE          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-07-07 MKL  ORIGINAL                                       *
      * 1994-12-15 BHS  PAID CUTOFF MOVED REQ5513                      *
      * 1995-10-28 BHS  ISSUED LIMIT REVISED PRB0730                   *
      * 1995-01-04 DPS  EXCEPT ROUNDING - HALF UP PRB8669              *
      * 1997-05-06 EWS  EXCEPT STATUS CODES ADDED PRB7933              *
      * 1997-05-06 SKN  ISSUED CALCULATION CORRECTED CHG4168           *
      * 1998-04-24 LMB  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-01-16 PQR  PERFORMANCE - STALE READ AHEAD INC8040         *
      * 1999-01-05 PQR  PERFORMANCE - ISSUED READ AHEAD CHG5849        *
      * 2001-03-05 EWS  NEW VOID REPORT COLUMNS REQ8283                *
      * 2002-02-03 GAF  EXCEPT EXCEPTIONS TO OPS QUEUE CHG4450         *
      * 2003-02-13 DPS  VOID TABLE EXPANDED INC5578                    *
      *================================================================*
       01  PPFEE-PARMS.
           05  PPFEE-STALE-YTD             PIC S9(13)V99 COMP-3.
           05  PPFEE-PAID-YTD              PIC S9(13)V99 COMP-3.
           05  PPFEE-EXCEPT-AMT            PIC S9(13)V99 COMP-3.
           05  PPFEE-VOID-YTD              PIC S9(13)V99 COMP-3.
           05  PPFEE-VOID-AMT              PIC S9(13)V99 COMP-3.
           05  PPFEE-PAID-AMT              PIC S9(13)V99 COMP-3.
           05  PPFEE-NEXT-REVIEW-DATE      PIC 9(08).
           05  PPFEE-LAST-PAY-DATE         PIC 9(08).
           05  PPFEE-CLOSE-DATE            PIC 9(08).
           05  PPFEE-OPEN-DATE             PIC 9(08).
           05  PPFEE-MATCH-RULE-CD         PIC X(01).
               88  PPFEE-MATCH-RULE-CD-A           VALUE 'A'.
               88  PPFEE-MATCH-RULE-CD-B           VALUE 'B'.
               88  PPFEE-MATCH-RULE-CD-F           VALUE 'F'.
               88  PPFEE-MATCH-RULE-CD-M           VALUE 'M'.
               88  PPFEE-MATCH-RULE-CD-S           VALUE 'S'.
               88  PPFEE-MATCH-RULE-CD-V           VALUE 'V'.
           05  PPFEE-REGION-CD             PIC X(01).
               88  PPFEE-REGION-CD-A               VALUE 'A'.
               88  PPFEE-REGION-CD-B               VALUE 'B'.
               88  PPFEE-REGION-CD-D               VALUE 'D'.
               88  PPFEE-REGION-CD-H               VALUE 'H'.
               88  PPFEE-REGION-CD-N               VALUE 'N'.
               88  PPFEE-REGION-CD-Y               VALUE 'Y'.
           05  PPFEE-STATUS-CD             PIC X(02).
               88  PPFEE-STATUS-CD-DR              VALUE 'DR'.
               88  PPFEE-STATUS-CD-TK              VALUE 'TK'.
               88  PPFEE-STATUS-CD-WX              VALUE 'WX'.
           05  PPFEE-REASON-CD             PIC X(01).
               88  PPFEE-REASON-CD-B               VALUE 'B'.
               88  PPFEE-REASON-CD-M               VALUE 'M'.
               88  PPFEE-REASON-CD-N               VALUE 'N'.
               88  PPFEE-REASON-CD-U               VALUE 'U'.
               88  PPFEE-REASON-CD-V               VALUE 'V'.
               88  PPFEE-REASON-CD-Z               VALUE 'Z'.
           05  PPFEE-NOTICE-SENT-FLAG      PIC X(01).
               88  PPFEE-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  PPFEE-EXEMPT-FLAG           PIC X(01).
               88  PPFEE-EXEMPT-FLAG-YES           VALUE 'Y'.
           05  PPFEE-OVERRIDE-FLAG         PIC X(01).
               88  PPFEE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  PPFEE-PAYMENT-CNT           PIC 9(07).
           05  PPFEE-NOTICE-CNT            PIC 9(07).
           05  PPFEE-DISCOUNT              PIC 9(03)V9(06).
           05  PPFEE-MARGIN                PIC 9(03)V9(06).
           05  PPFEE-CONTACT               PIC X(20).
           05  PPFEE-REF-NO                PIC X(16).
           05  PPFEE-RETURN-CODE           PIC 9(02).
