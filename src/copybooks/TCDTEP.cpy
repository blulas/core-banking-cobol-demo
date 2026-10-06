      *================================================================*
      * COPYBOOK : TCDTEP                                              *
      * PURPOSE  : LINKAGE FOR TRAVELERS CHEQUES RULE MODULE TCDTE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-03-01 RJT  ORIGINAL                                       *
      * 1998-03-20 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-10-18 EWS  NEW COMMISSION REPORT COLUMNS INC4467          *
      * 2009-09-26 GAF  SHORTAGE TABLE EXPANDED PRB8479                *
      * 2013-03-21 BHS  REFUND CALCULATION CORRECTED INC3382           *
      * 2013-02-22 CRP  AUDIT FINDING - SHORTAGE CONTROL TOTALS REQ6968*
      * 2014-06-04 LMB  FACE FEE SCHEDULE UPDATE INC5637               *
      *================================================================*
       01  TCDTE-PARMS.
           05  TCDTE-SHORTAGE-AMT          PIC S9(13)V99 COMP-3.
           05  TCDTE-COMMISSION-YTD        PIC S9(13)V99 COMP-3.
           05  TCDTE-REFUND-YTD            PIC S9(13)V99 COMP-3.
           05  TCDTE-SHORTAGE-YTD          PIC S9(13)V99 COMP-3.
           05  TCDTE-CONSIGN-AMT           PIC S9(13)V99 COMP-3.
           05  TCDTE-REFUND-AMT            PIC S9(13)V99 COMP-3.
           05  TCDTE-MATURITY-DATE         PIC 9(08).
           05  TCDTE-CLOSE-DATE            PIC 9(08).
           05  TCDTE-NEXT-DUE-DATE         PIC 9(08).
           05  TCDTE-LAST-ACT-DATE         PIC 9(08).
           05  TCDTE-BRANCH-CD             PIC X(02).
               88  TCDTE-BRANCH-CD-EG              VALUE 'EG'.
               88  TCDTE-BRANCH-CD-HZ              VALUE 'HZ'.
               88  TCDTE-BRANCH-CD-JF              VALUE 'JF'.
               88  TCDTE-BRANCH-CD-NZ              VALUE 'NZ'.
           05  TCDTE-ISSUER-CD             PIC X(02).
               88  TCDTE-ISSUER-CD-AF              VALUE 'AF'.
               88  TCDTE-ISSUER-CD-CJ              VALUE 'CJ'.
               88  TCDTE-ISSUER-CD-DU              VALUE 'DU'.
               88  TCDTE-ISSUER-CD-JY              VALUE 'JY'.
               88  TCDTE-ISSUER-CD-RR              VALUE 'RR'.
               88  TCDTE-ISSUER-CD-TK              VALUE 'TK'.
           05  TCDTE-CYCLE-CD              PIC X(03).
           05  TCDTE-STATUS-CD             PIC X(02).
               88  TCDTE-STATUS-CD-HK              VALUE 'HK'.
               88  TCDTE-STATUS-CD-RU              VALUE 'RU'.
               88  TCDTE-STATUS-CD-WA              VALUE 'WA'.
           05  TCDTE-STOP-FLAG             PIC X(01).
               88  TCDTE-STOP-FLAG-YES             VALUE 'Y'.
           05  TCDTE-OVERRIDE-FLAG         PIC X(01).
               88  TCDTE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  TCDTE-REVIEW-FLAG           PIC X(01).
               88  TCDTE-REVIEW-FLAG-YES           VALUE 'Y'.
           05  TCDTE-EXCEPT-CNT            PIC 9(07).
           05  TCDTE-ITEM-CNT              PIC 9(07).
           05  TCDTE-RATE                  PIC 9(03)V9(06).
           05  TCDTE-MARGIN                PIC 9(03)V9(06).
           05  TCDTE-REF-NO                PIC X(16).
           05  TCDTE-CUST-ID               PIC X(10).
           05  TCDTE-RETURN-CODE           PIC 9(02).
