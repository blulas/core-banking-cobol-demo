      *================================================================*
      * COPYBOOK : GLREC                                               *
      * PURPOSE  : GENERAL LEDGER JOURNAL ENTRY                        *
      * FILE     : GLJRNL (SEQUENTIAL, WRITTEN BY GLEXTR, POSTED BY    *
      *            GLTRIAL). EVERY SOURCE TRANSACTION PRODUCES ONE     *
      *            DEBIT AND ONE CREDIT ENTRY OF EQUAL AMOUNT.         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2006-02-14 BHS  GL ACCOUNT EXPANDED TO X(10) FOR ORACLE GL     *
      * 2015-10-02 DPS  SUSPENSE FLAG                                  *
      *================================================================*
       01  GL-JOURNAL-RECORD.
           05  GLJ-DATE                    PIC 9(08).
           05  GLJ-JOURNAL-SEQ             PIC 9(08).
           05  GLJ-GL-ACCOUNT              PIC X(10).
           05  GLJ-COST-CENTER             PIC X(05).
           05  GLJ-DR-CR                   PIC X(01).
               88  GLJ-DEBIT                         VALUE 'D'.
               88  GLJ-CREDIT                        VALUE 'C'.
           05  GLJ-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  GLJ-SOURCE-PGM              PIC X(08).
           05  GLJ-SOURCE-TRAN-ID          PIC X(16).
           05  GLJ-ACCT-NUMBER             PIC X(12).
           05  GLJ-TRAN-CODE               PIC X(04).
           05  GLJ-DESCRIPTION             PIC X(40).
           05  GLJ-SUSPENSE-FLAG           PIC X(01).
               88  GLJ-IN-SUSPENSE                   VALUE 'Y'.
           05  FILLER                      PIC X(20).
