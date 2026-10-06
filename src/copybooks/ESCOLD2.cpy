      *================================================================*
      * COPYBOOK : ESCOLD2                                             *
      * PURPOSE  : MORTGAGE ESCROW PRIOR VERSION RECORD LAYOUT         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1989-11-16 SKN  ORIGINAL                                       *
      * 1993-04-16 SKN  TAX ROUNDING - HALF UP REQ3914                 *
      * 1998-05-15 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2000-11-27 SKN  SHORTAGE NOTICE TEXT PER LEGAL CHG4008         *
      * 2000-04-01 DPS  TAX EXCEPTIONS TO OPS QUEUE PRB0285            *
      * 2000-12-23 RJT  TAX FEE SCHEDULE UPDATE PRB7116                *
      * 2001-03-28 TWH  INSURANCE RERUN PROTECTION INC7879             *
      * 2003-06-27 CRP  CUSHION RERUN PROTECTION PRB7027               *
      * 2008-08-06 BHS  DISBURSE ROUNDING - HALF UP PRB7495            *
      *================================================================*
       01  ESC-H2-REC.
           05  ESCH2-DESC                  PIC X(30).
           05  ESCH2-KEY                   PIC X(12).
           05  ESCH2-CONTACT               PIC X(20).
           05  ESCH2-ADDR                  PIC X(30).
           05  ESCH2-CUST-ID               PIC X(10).
           05  ESCH2-NAME                  PIC X(30).
           05  ESC-H2-AMOUNTS.
               10  ESCH2-TAX-YTD           PIC S9(13)V99 COMP-3.
               10  ESCH2-INSURANCE-AMT     PIC S9(13)V99 COMP-3.
               10  ESCH2-SURPLUS-AMT       PIC S9(13)V99 COMP-3.
               10  ESCH2-DISBURSE-YTD      PIC S9(13)V99 COMP-3.
               10  ESCH2-CUSHION-YTD       PIC S9(13)V99 COMP-3.
               10  ESCH2-SHORTAGE-AMT      PIC S9(13)V99 COMP-3.
               10  ESCH2-CUSHION-AMT       PIC S9(13)V99 COMP-3.
               10  ESCH2-PAYMENT-AMT       PIC S9(13)V99 COMP-3.
               10  ESCH2-PAYMENT-YTD       PIC S9(13)V99 COMP-3.
               10  ESCH2-DISBURSE-AMT      PIC S9(13)V99 COMP-3.
               10  ESCH2-SHORTAGE-YTD      PIC S9(13)V99 COMP-3.
               10  ESCH2-SURPLUS-YTD       PIC S9(13)V99 COMP-3.
           05  ESC-H2-DATES.
               10  ESCH2-CLOSE-DATE        PIC 9(08).
               10  ESCH2-LAST-PAY-DATE     PIC 9(08).
               10  ESCH2-EFFECTIVE-DATE    PIC 9(08).
               10  ESCH2-NEXT-DUE-DATE     PIC 9(08).
               10  ESCH2-NEXT-REVIEW-DATE  PIC 9(08).
               10  ESCH2-LAST-ACT-DATE     PIC 9(08).
               10  ESCH2-NOTICE-DATE       PIC 9(08).
           05  ESC-H2-CODES.
               10  ESCH2-OVERRIDE-FLAG     PIC X(01).
                   88  ESCH2-OVERRIDE-FLAG-YES     VALUE 'Y'.
               10  ESCH2-STATUS-CD         PIC X(01).
                   88  ESCH2-STATUS-CD-A           VALUE 'A'.
                   88  ESCH2-STATUS-CD-E           VALUE 'E'.
                   88  ESCH2-STATUS-CD-R           VALUE 'R'.
                   88  ESCH2-STATUS-CD-U           VALUE 'U'.
                   88  ESCH2-STATUS-CD-Z           VALUE 'Z'.
               10  ESCH2-SUSPEND-FLAG      PIC X(01).
                   88  ESCH2-SUSPEND-FLAG-YES      VALUE 'Y'.
               10  ESCH2-REGION-CD         PIC X(01).
                   88  ESCH2-REGION-CD-B           VALUE 'B'.
                   88  ESCH2-REGION-CD-G           VALUE 'G'.
                   88  ESCH2-REGION-CD-Y           VALUE 'Y'.
               10  ESCH2-NOTICE-SENT-FLAG  PIC X(01).
                   88  ESCH2-NOTICE-SENT-FLAG-YES  VALUE 'Y'.
               10  ESCH2-ANALYSIS-CD       PIC X(02).
                   88  ESCH2-ANALYSIS-CD-GD        VALUE 'GD'.
                   88  ESCH2-ANALYSIS-CD-LY        VALUE 'LY'.
                   88  ESCH2-ANALYSIS-CD-XJ        VALUE 'XJ'.
               10  ESCH2-PREFERRED-FLAG    PIC X(01).
                   88  ESCH2-PREFERRED-FLAG-YES    VALUE 'Y'.
               10  ESCH2-PAYEE-TYPE-CD     PIC X(02).
                   88  ESCH2-PAYEE-TYPE-CD-AM      VALUE 'AM'.
                   88  ESCH2-PAYEE-TYPE-CD-UE      VALUE 'UE'.
                   88  ESCH2-PAYEE-TYPE-CD-UP      VALUE 'UP'.
               10  ESCH2-HOLD-FLAG         PIC X(01).
                   88  ESCH2-HOLD-FLAG-YES         VALUE 'Y'.
               10  ESCH2-REASON-CD         PIC X(01).
                   88  ESCH2-REASON-CD-C           VALUE 'C'.
                   88  ESCH2-REASON-CD-F           VALUE 'F'.
                   88  ESCH2-REASON-CD-K           VALUE 'K'.
                   88  ESCH2-REASON-CD-L           VALUE 'L'.
                   88  ESCH2-REASON-CD-S           VALUE 'S'.
                   88  ESCH2-REASON-CD-X           VALUE 'X'.
               10  ESCH2-TYPE-CD           PIC X(03).
           05  ESC-H2-COUNTERS.
               10  ESCH2-FEE-PCT           PIC 9(03)V9(06).
               10  ESCH2-EXCEPT-CNT        PIC 9(07).
               10  ESCH2-RETRY-CNT         PIC 9(07).
               10  ESCH2-RATE              PIC 9(03)V9(06).
               10  ESCH2-NOTICE-CNT        PIC 9(07).
               10  ESCH2-CYCLE-CNT         PIC 9(07).
               10  ESCH2-DISCOUNT          PIC 9(03)V9(06).
           05  FILLER                      PIC X(20).
