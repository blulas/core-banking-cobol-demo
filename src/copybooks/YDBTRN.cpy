      *================================================================*
      * COPYBOOK : YDBTRN                                              *
      * PURPOSE  : DATE WINDOW BRIDGE DAILY TRANSACTION RECORD         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-06-06 LMB  ORIGINAL                                       *
      * 1992-08-07 PQR  CONTROL LIMIT REVISED CHG9553                  *
      * 1993-12-27 EWS  CONTROL FEE SCHEDULE UPDATE PRB6376            *
      * 1998-08-10 EWS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-05-02 SKN  AMOUNT-2 FEE SCHEDULE UPDATE CHG5507           *
      * 1999-01-08 TWH  PERFORMANCE - CONTROL READ AHEAD PRB2326       *
      * 2001-06-07 BHS  CONTROL NOTICE TEXT PER LEGAL PRB6871          *
      * 2002-04-22 SKN  CONTROL TABLE EXPANDED CHG6984                 *
      * 2002-05-12 PQR  CONTROL TABLE EXPANDED CHG6327                 *
      * 2007-02-13 JLM  CONTROL CALCULATION CORRECTED PRB5844          *
      *================================================================*
       01  YDB-TRAN-REC.
           05  YDBT-KEY                    PIC X(12).
           05  YDBT-OPERATOR               PIC X(08).
           05  YDBT-REJECT-CD              PIC X(04).
           05  YDBT-MEMO                   PIC X(30).
           05  YDB-TRAN-AMOUNTS.
               10  YDBT-AMOUNT-1-AMT       PIC S9(13)V99 COMP-3.
               10  YDBT-AMOUNT-2-AMT       PIC S9(13)V99 COMP-3.
               10  YDBT-CONTROL-AMT        PIC S9(13)V99 COMP-3.
           05  YDB-TRAN-DATES.
               10  YDBT-EFF-DATE           PIC 9(08).
               10  YDBT-POST-DATE          PIC 9(08).
           05  YDB-TRAN-CODES.
               10  YDBT-TRAN-CD            PIC X(02).
                   88  YDBT-TRAN-CD-CV             VALUE 'CV'.
                   88  YDBT-TRAN-CD-RV             VALUE 'RV'.
                   88  YDBT-TRAN-CD-TS             VALUE 'TS'.
               10  YDBT-CHANNEL-CD         PIC X(03).
                   88  YDBT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  YDBT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  YDBT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  YDBT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  YDBT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  YDBT-REVERSAL-FLAG      PIC X(01).
                   88  YDBT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  YDB-TRAN-COUNTERS.
               10  YDBT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
