      *================================================================*
      * COPYBOOK : CTRREC                                              *
      * PURPOSE  : CURRENCY TRANSACTION REPORT (CTR) RECORD - ONE PER  *
      *            PERSON CONDUCTING PER BUSINESS DAY. LAYOUT FOLLOWS  *
      *            FINCEN FORM 112 (BSA E-FILING) PART I / II / III.   *
      *            PICKED UP BY THE BSA E-FILING EXTRACT (CBSBSAEF).   *
      * FILE     : CTRFILE  (SEQUENTIAL, FIXED, WRITTEN BY CTRSCAN)    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-04-01 RJT  ORIGINAL - FORM 4789 LAYOUT                    *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-10-21 BHS  MULTIPLE ACCOUNTS PER CTR (USA PATRIOT ACT)    *
      * 2012-03-30 DPS  REG-2012-F FINCEN 112 BSA E-FILING LAYOUT      *
      * 2019-10-07 AKW  EXEMPTION STATUS / MSB INDICATOR               *
      *================================================================*
       01  CTR-RECORD.
           05  CTR-REPORT-ID.
               10  CTR-REPORT-DATE         PIC 9(08).
               10  CTR-REPORT-SEQ          PIC 9(06).
           05  CTR-FILING-TYPE             PIC X(01).
               88  CTR-INITIAL-REPORT                VALUE 'I'.
               88  CTR-CORRECTED-REPORT              VALUE 'C'.
      *--- PART IV - FILING INSTITUTION -------------------------------*
           05  CTR-FILING-INSTITUTION.
               10  CTR-FI-BANK-ID          PIC X(04).
               10  CTR-FI-NAME             PIC X(35).
               10  CTR-FI-EIN              PIC X(09).
               10  CTR-FI-RSSD             PIC X(10).
               10  CTR-FI-REGULATOR        PIC X(01).
      *            1=OCC 2=FDIC 3=FRB 4=NCUA 7=IRS
      *--- PART II - TRANSACTION -------------------------------------*
           05  CTR-TRAN-DATE               PIC 9(08).
           05  CTR-FILING-DUE-DATE         PIC 9(08).
      *--- PART I - PERSON CONDUCTING / ON WHOSE BEHALF ---------------*
           05  CTR-PERSON.
               10  CTR-CUST-ID             PIC X(10).
               10  CTR-PERSON-TYPE         PIC X(01).
                   88  CTR-PERSON-INDIVIDUAL         VALUE 'I'.
                   88  CTR-PERSON-ENTITY             VALUE 'E'.
               10  CTR-LAST-NAME           PIC X(30).
               10  CTR-FIRST-NAME          PIC X(20).
               10  CTR-MIDDLE-INIT         PIC X(01).
               10  CTR-TAX-ID              PIC X(09).
               10  CTR-TAX-ID-TYPE         PIC X(01).
               10  CTR-BIRTH-DATE          PIC 9(08).
               10  CTR-ADDR-LINE1          PIC X(35).
               10  CTR-CITY                PIC X(25).
               10  CTR-STATE               PIC X(02).
               10  CTR-ZIP                 PIC X(10).
               10  CTR-COUNTRY             PIC X(02).
               10  CTR-OCCUPATION          PIC X(06).
      *--- PART II - AMOUNTS (CASH IN / CASH OUT - NEVER NETTED) ------*
           05  CTR-CASH-IN-TOTAL           PIC S9(13)V99 COMP-3.
           05  CTR-CASH-OUT-TOTAL          PIC S9(13)V99 COMP-3.
           05  CTR-CASH-IN-REPORTABLE      PIC X(01).
           05  CTR-CASH-OUT-REPORTABLE     PIC X(01).
           05  CTR-TRAN-COUNT              PIC 9(04).
           05  CTR-AGGREGATED-FLAG         PIC X(01).
               88  CTR-MULTIPLE-TRANSACTIONS         VALUE 'Y'.
           05  CTR-MULTI-BRANCH-FLAG       PIC X(01).
               88  CTR-MULTIPLE-BRANCHES             VALUE 'Y'.
           05  CTR-ARMORED-CAR-FLAG        PIC X(01).
           05  CTR-EXEMPT-STATUS           PIC X(01).
               88  CTR-NOT-EXEMPT                    VALUE 'N'.
               88  CTR-EXEMPT-PHASE-I                VALUE 'E'.
               88  CTR-EXEMPT-INELIGIBLE             VALUE 'I'.
           05  CTR-MSB-FLAG                PIC X(01).
           05  CTR-ACCOUNT-COUNT           PIC 9(02).
           05  CTR-ACCOUNT-ENTRY OCCURS 6 TIMES.
               10  CTR-ACCT-NUMBER         PIC X(12).
               10  CTR-ACCT-CASH-IN        PIC S9(11)V99 COMP-3.
               10  CTR-ACCT-CASH-OUT       PIC S9(11)V99 COMP-3.
           05  CTR-STATUS                  PIC X(01).
               88  CTR-PENDING-FILING                VALUE 'P'.
               88  CTR-FILED                         VALUE 'F'.
           05  CTR-SOURCE-PGM              PIC X(08).
           05  FILLER                      PIC X(30).
