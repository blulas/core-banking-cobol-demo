      *================================================================*
      * COPYBOOK : ESCARC1                                             *
      * PURPOSE  : MORTGAGE ESCROW PRIOR VERSION RECORD LAYOUT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-06-23 PQR  ORIGINAL                                       *
      * 1992-12-13 PQR  PERFORMANCE - SHORTAGE READ AHEAD PRB0732      *
      * 1996-05-02 TWH  SHORTAGE ROUNDING - HALF UP CHG9792            *
      * 1997-09-09 TWH  CUSHION LIMIT REVISED REQ7814                  *
      * 2000-02-25 BHS  PAYMENT CALCULATION CORRECTED CHG2611          *
      * 2003-07-02 CRP  INSURANCE RERUN PROTECTION INC4125             *
      * 2006-06-16 JLM  PERFORMANCE - TAX READ AHEAD CHG9063           *
      * 2013-07-18 GAF  CUSHION FEE SCHEDULE UPDATE CHG0445            *
      *================================================================*
       01  ESC-H1-REC.
           05  ESCH1-ADDR                  PIC X(30).
           05  ESCH1-REF-NO                PIC X(16).
           05  ESCH1-CUST-ID               PIC X(10).
           05  ESCH1-KEY                   PIC X(12).
           05  ESCH1-CONTACT               PIC X(20).
           05  ESCH1-DESC                  PIC X(30).
           05  ESC-H1-AMOUNTS.
               10  ESCH1-SURPLUS-YTD       PIC S9(13)V99 COMP-3.
               10  ESCH1-CUSHION-YTD       PIC S9(13)V99 COMP-3.
               10  ESCH1-PAYMENT-AMT       PIC S9(13)V99 COMP-3.
               10  ESCH1-SHORTAGE-AMT      PIC S9(13)V99 COMP-3.
               10  ESCH1-SHORTAGE-YTD      PIC S9(13)V99 COMP-3.
               10  ESCH1-INSURANCE-YTD     PIC S9(13)V99 COMP-3.
               10  ESCH1-TAX-AMT           PIC S9(13)V99 COMP-3.
               10  ESCH1-TAX-YTD           PIC S9(13)V99 COMP-3.
               10  ESCH1-DISBURSE-AMT      PIC S9(13)V99 COMP-3.
               10  ESCH1-SURPLUS-AMT       PIC S9(13)V99 COMP-3.
               10  ESCH1-PAYMENT-YTD       PIC S9(13)V99 COMP-3.
               10  ESCH1-DISBURSE-YTD      PIC S9(13)V99 COMP-3.
           05  ESC-H1-DATES.
               10  ESCH1-LAST-ACT-DATE     PIC 9(08).
               10  ESCH1-LAST-STMT-DATE    PIC 9(08).
               10  ESCH1-LAST-PAY-DATE     PIC 9(08).
               10  ESCH1-NEXT-REVIEW-DATE  PIC 9(08).
               10  ESCH1-NEXT-DUE-DATE     PIC 9(08).
               10  ESCH1-NOTICE-DATE       PIC 9(08).
               10  ESCH1-EFFECTIVE-DATE    PIC 9(08).
               10  ESCH1-EXPIRY-DATE       PIC 9(08).
               10  ESCH1-CLOSE-DATE        PIC 9(08).
           05  ESC-H1-CODES.
               10  ESCH1-NOTICE-SENT-FLAG  PIC X(01).
                   88  ESCH1-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  ESCH1-STATUS-CD         PIC X(01).
                   88  ESCH1-STATUS-CD-A           VALUE 'A'.
                   88  ESCH1-STATUS-CD-E           VALUE 'E'.
                   88  ESCH1-STATUS-CD-R           VALUE 'R'.
                   88  ESCH1-STATUS-CD-U           VALUE 'U'.
                   88  ESCH1-STATUS-CD-Z           VALUE 'Z'.
               10  ESCH1-PREFERRED-FLAG    PIC X(01).
                   88  ESCH1-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  ESCH1-TYPE-CD           PIC X(03).
               10  ESCH1-HOLD-FLAG         PIC X(01).
                   88  ESCH1-HOLD-FLAG-YES         VALUE 'Y'.
               10  ESCH1-ANALYSIS-CD       PIC X(02).
                   88  ESCH1-ANALYSIS-CD-GD        VALUE 'GD'.
                   88  ESCH1-ANALYSIS-CD-LY        VALUE 'LY'.
                   88  ESCH1-ANALYSIS-CD-XJ        VALUE 'XJ'.
               10  ESCH1-PAYEE-TYPE-CD     PIC X(02).
                   88  ESCH1-PAYEE-TYPE-CD-AM      VALUE 'AM'.
                   88  ESCH1-PAYEE-TYPE-CD-UE      VALUE 'UE'.
                   88  ESCH1-PAYEE-TYPE-CD-UP      VALUE 'UP'.
               10  ESCH1-OVERRIDE-FLAG     PIC X(01).
                   88  ESCH1-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  ESCH1-REASON-CD         PIC X(01).
                   88  ESCH1-REASON-CD-C           VALUE 'C'.
                   88  ESCH1-REASON-CD-F           VALUE 'F'.
                   88  ESCH1-REASON-CD-K           VALUE 'K'.
                   88  ESCH1-REASON-CD-L           VALUE 'L'.
                   88  ESCH1-REASON-CD-S           VALUE 'S'.
                   88  ESCH1-REASON-CD-X           VALUE 'X'.
               10  ESCH1-STOP-FLAG         PIC X(01).
                   88  ESCH1-STOP-FLAG-YES         VALUE 'Y'.
               10  ESCH1-SUSPEND-FLAG      PIC X(01).
                   88  ESCH1-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  ESCH1-REGION-CD         PIC X(01).
                   88  ESCH1-REGION-CD-B           VALUE 'B'.
                   88  ESCH1-REGION-CD-G           VALUE 'G'.
                   88  ESCH1-REGION-CD-Y           VALUE 'Y'.
           05  ESC-H1-COUNTERS.
               10  ESCH1-RATE              PIC 9(03)V9(06).
               10  ESCH1-EXCEPT-CNT        PIC 9(07).
               10  ESCH1-CYCLE-CNT         PIC 9(07).
               10  ESCH1-DISCOUNT          PIC 9(03)V9(06).
               10  ESCH1-RETRY-CNT         PIC 9(07).
               10  ESCH1-NOTICE-CNT        PIC 9(07).
               10  ESCH1-FEE-PCT           PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
