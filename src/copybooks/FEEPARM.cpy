      *================================================================*
      * COPYBOOK : FEEPARM                                             *
      * PURPOSE  : LINKAGE FOR FEECALC - DEPOSIT SERVICE FEE AMOUNTS   *
      *   SHARED INTERFACE - USED BY FEEASSES, POSTING AND TXB.        *
      *   CALLER SETS FEE-TYPE, PRODUCT, SEGMENT AND (OPTIONAL) ITEM   *
      *   COUNT; FEECALC RETURNS THE FEE, THE TRANSACTION CODE TO      *
      *   POST IT UNDER AND A DESCRIPTION FOR THE STATEMENT.           *
      *   FEE-TYPE   MNTH MONTHLY MAINTENANCE (BY PRODUCT)             *
      *              WDOM OUTGOING DOMESTIC WIRE                       *
      *              WINT OUTGOING INTERNATIONAL WIRE                  *
      *              WINC INCOMING WIRE                                *
      *              PSTM PAPER STATEMENT (FEE-ESTMT-FLAG NOT 'Y')     *
      *              STOP STOP PAYMENT                                 *
      *   FEE-ITEM-COUNT  ZERO IS TREATED AS 1. AMOUNT = UNIT * COUNT. *
      *   RETURN CODES: 00 FEE APPLIES  04 NO FEE FOR THIS PRODUCT /   *
      *                 SEGMENT (AMOUNT ZERO)  12 UNKNOWN FEE TYPE     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL - MAINTENANCE FEE ONLY                *
      * 2004-03-22 BHS  WIRE AND STOP PAYMENT FEES                     *
      * 2010-07-01 DPS  REG E - FEE TRAN CODE RETURNED PER FEE TYPE    *
      * 2018-01-15 AKW  PAPER STATEMENT FEE - CHG1502                  *
      *================================================================*
       01  FEECALC-PARMS.
           05  FEE-TYPE                    PIC X(04).
               88  FEE-MAINTENANCE                   VALUE 'MNTH'.
               88  FEE-WIRE-DOMESTIC                 VALUE 'WDOM'.
               88  FEE-WIRE-INTL                     VALUE 'WINT'.
               88  FEE-WIRE-INCOMING                 VALUE 'WINC'.
               88  FEE-PAPER-STMT                    VALUE 'PSTM'.
               88  FEE-STOP-PAYMENT                  VALUE 'STOP'.
           05  FEE-PRODUCT-CODE            PIC X(04).
           05  FEE-CUST-SEGMENT            PIC X(02).
           05  FEE-ESTMT-FLAG              PIC X(01).
           05  FEE-ITEM-COUNT              PIC 9(03).
      *--- OUTPUT ----------------------------------------------------*
           05  FEE-AMOUNT                  PIC S9(07)V99 COMP-3.
           05  FEE-TRAN-CODE               PIC X(04).
           05  FEE-DESCRIPTION             PIC X(30).
           05  FEE-RETURN-CODE             PIC 9(02).
