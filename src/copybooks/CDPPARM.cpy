      *================================================================*
      * COPYBOOK : CDPPARM                                             *
      * PURPOSE  : LINKAGE FOR CDPENLTY - CD EARLY WITHDRAWAL PENALTY  *
      *   INPUT : PRODUCT, PRODUCT PENALTY DAYS (PROD-EARLY-WD-        *
      *           PENALTY-DAYS), CURRENT PRINCIPAL, AMOUNT TO WITHDRAW *
      *           (ZERO = FULL), RATE, FUNDING / MATURITY / GRACE END  *
      *           DATES, WITHDRAWAL DATE, UNPAID ACCRUED INTEREST AND  *
      *           THE DECEASED-OWNER FLAG.                             *
      *   OUTPUT: PENALTY, SPLIT INTO THE PART TAKEN FROM INTEREST     *
      *           AND THE PART TAKEN FROM PRINCIPAL, NET PROCEEDS AND  *
      *           A WAIVER CODE WHEN NO PENALTY APPLIES:               *
      *             MT AT OR AFTER MATURITY       GP IN GRACE PERIOD   *
      *             DC OWNER DECEASED             NP NO-PENALTY CD     *
      *   RETURN CODES: 00 PENALTY COMPUTED   04 PENALTY WAIVED        *
      *                 08 WITHDRAWAL EXCEEDS PRINCIPAL  12 BAD INPUT  *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 2009-04-20 DPS  REMOVED IRA AGE 59 1/2 / 70 1/2 WAIVERS - IRA  *
      *                 CDS MIGRATED TO TRUST PLATFORM (CHG0987)       *
      * 2017-02-08 AKW  NO-PENALTY CD (CDNP)                           *
      *================================================================*
       01  CDPENLTY-PARMS.
           05  CDP-PRODUCT-CODE            PIC X(04).
           05  CDP-PENALTY-DAYS            PIC 9(03).
           05  CDP-PRINCIPAL               PIC S9(13)V99 COMP-3.
           05  CDP-WITHDRAWAL-AMT          PIC S9(13)V99 COMP-3.
           05  CDP-INT-RATE                PIC 9(03)V9(06).
           05  CDP-FUNDING-DATE            PIC 9(08).
           05  CDP-MATURITY-DATE           PIC 9(08).
           05  CDP-GRACE-END-DATE          PIC 9(08).
           05  CDP-WITHDRAWAL-DATE         PIC 9(08).
           05  CDP-ACCRUED-INT             PIC S9(09)V9(06) COMP-3.
           05  CDP-OWNER-DECEASED          PIC X(01).
      *--- OUTPUT ----------------------------------------------------*
           05  CDP-DAYS-APPLIED            PIC 9(03).
           05  CDP-PENALTY-AMT             PIC S9(13)V99 COMP-3.
           05  CDP-PENALTY-FROM-INT        PIC S9(13)V99 COMP-3.
           05  CDP-PENALTY-FROM-PRIN       PIC S9(13)V99 COMP-3.
           05  CDP-NET-PROCEEDS            PIC S9(13)V99 COMP-3.
           05  CDP-WAIVER-CODE             PIC X(02).
           05  CDP-RETURN-CODE             PIC 9(02).
