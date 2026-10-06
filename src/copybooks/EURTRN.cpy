      *================================================================*
      * COPYBOOK : EURTRN                                              *
      * PURPOSE  : EURO CONVERSION DAILY TRANSACTION RECORD            *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-08-19 MKL  ORIGINAL                                       *
      * 1997-11-09 GAF  NATIONAL ROUNDING - HALF UP INC4150            *
      * 2000-10-23 EWS  RESIDUAL CUTOFF MOVED INC9774                  *
      * 2001-10-10 MKL  ROUNDING RERUN PROTECTION REQ7482              *
      * 2003-04-01 RJT  PERFORMANCE - TRIANGULAR READ AHEAD REQ7848    *
      * 2003-10-04 PQR  EURO EXCEPTIONS TO OPS QUEUE PRB5976           *
      * 2004-12-09 RJT  TRIANGULAR CALCULATION CORRECTED CHG5058       *
      * 2005-01-25 PQR  CONVERTED EXCEPTIONS TO OPS QUEUE CHG4293      *
      * 2006-06-23 LMB  NEW CONVERTED REPORT COLUMNS REQ2948           *
      * 2008-05-04 RJT  NEW CONVERTED REPORT COLUMNS PRB0116           *
      * 2010-08-25 LMB  NATIONAL LIMIT REVISED PRB5483                 *
      *================================================================*
       01  EUR-TRAN-REC.
           05  EURT-KEY                    PIC X(12).
           05  EURT-OPERATOR               PIC X(08).
           05  EURT-REJECT-CD              PIC X(04).
           05  EURT-MEMO                   PIC X(30).
           05  EUR-TRAN-AMOUNTS.
               10  EURT-CONVERTED-AMT      PIC S9(13)V99 COMP-3.
               10  EURT-ROUNDING-AMT       PIC S9(13)V99 COMP-3.
               10  EURT-FEE-AMT            PIC S9(13)V99 COMP-3.
               10  EURT-NATIONAL-AMT       PIC S9(13)V99 COMP-3.
           05  EUR-TRAN-DATES.
               10  EURT-EFF-DATE           PIC 9(08).
               10  EURT-POST-DATE          PIC 9(08).
           05  EUR-TRAN-CODES.
               10  EURT-TRAN-CD            PIC X(02).
                   88  EURT-TRAN-CD-CV             VALUE 'CV'.
                   88  EURT-TRAN-CD-RV             VALUE 'RV'.
                   88  EURT-TRAN-CD-AD             VALUE 'AD'.
                   88  EURT-TRAN-CD-RD             VALUE 'RD'.
                   88  EURT-TRAN-CD-FX             VALUE 'FX'.
               10  EURT-CHANNEL-CD         PIC X(03).
                   88  EURT-CHANNEL-CD-BRN         VALUE 'BRN'.
                   88  EURT-CHANNEL-CD-BAT         VALUE 'BAT'.
                   88  EURT-CHANNEL-CD-MAL         VALUE 'MAL'.
                   88  EURT-CHANNEL-CD-TEL         VALUE 'TEL'.
                   88  EURT-CHANNEL-CD-ONL         VALUE 'ONL'.
               10  EURT-REVERSAL-FLAG      PIC X(01).
                   88  EURT-REVERSAL-FLAG-YES      VALUE 'Y'.
           05  EUR-TRAN-COUNTERS.
               10  EURT-SEQ                PIC 9(07).
           05  FILLER                      PIC X(30).
