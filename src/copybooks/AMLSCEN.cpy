      *================================================================*
      * COPYBOOK : AMLSCEN                                             *
      * PURPOSE  : AML TRANSACTION MONITORING SCENARIO TABLE AND       *
      *            HIGH-RISK GEOGRAPHY TABLE. THRESHOLDS ARE SET BY    *
      *            THE BSA OFFICER AND APPROVED BY THE BSA/AML         *
      *            COMMITTEE. CHANGES REQUIRE A CHG TICKET AND MODEL   *
      *            VALIDATION SIGN-OFF (SR 11-7).                      *
      *   SCEN-PCT  : PERCENT OR MULTIPLE DEPENDING ON SCENARIO        *
      *               AML1 = MIN PCT OF CREDIT MOVED OUT (90)          *
      *               AML4 = SPIKE MULTIPLE OF 30-DAY AVG X 100 (300)  *
      *               AML7 = CREDITS AS PCT OF MONTHLY INCOME (200)    *
      *   SCEN-SEGMENTS : UP TO 5 CUST-SEGMENT CODES, '**' = ALL       *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2004-08-02 BHS  ORIGINAL - AML1 TO AML3                        *
      * 2008-02-25 BHS  FATF HIGH-RISK LIST ADDED                      *
      * 2011-01-18 DPS  AML4 MSB SPIKE, AML5 DORMANT REACTIVATION      *
      * 2013-09-30 DPS  CHG0874 AML2 THRESHOLD 10,000 TO 3,000         *
      *                 (TRAVEL RULE ALIGNMENT)                        *
      * 2015-05-27 DPS  AML6 ROUND WIRE NEW BENEFICIARY                *
      * 2018-05-11 AKW  CDD RULE - AML7 PROFILE MISMATCH               *
      * 2024-03-11 AKW  CHG2291 FATF GREY LIST REFRESH (FEB 2024)      *
      * 2025-07-02 AKW  CHG2460 FATF GREY LIST REFRESH (JUN 2025)      *
      *================================================================*
       01  AML-SCENARIO-VALUES.
      *        ID    RULE-ID   DESCRIPTION
      *        LOOKBACK AMOUNT  COUNT PCT  SEGMENTS  SEV ACTIVE
           05  FILLER.
               10  FILLER PIC X(04)    VALUE 'AML1'.
               10  FILLER PIC X(08)    VALUE 'AMLRAPID'.
               10  FILLER PIC X(40)    VALUE
                   'RAPID MOVEMENT OF FUNDS'.
               10  FILLER PIC 9(03)    VALUE 003.
               10  FILLER PIC 9(09)V99 VALUE 25000.00.
               10  FILLER PIC 9(03)    VALUE 001.
               10  FILLER PIC 9(03)    VALUE 090.
               10  FILLER PIC X(10)    VALUE '**'.
               10  FILLER PIC X(01)    VALUE 'H'.
               10  FILLER PIC X(01)    VALUE 'Y'.
           05  FILLER.
               10  FILLER PIC X(04)    VALUE 'AML2'.
               10  FILLER PIC X(08)    VALUE 'AMLHRGEO'.
               10  FILLER PIC X(40)    VALUE
                   'HIGH-RISK GEOGRAPHY WIRE'.
               10  FILLER PIC 9(03)    VALUE 000.
               10  FILLER PIC 9(09)V99 VALUE 3000.00.
               10  FILLER PIC 9(03)    VALUE 001.
               10  FILLER PIC 9(03)    VALUE 000.
               10  FILLER PIC X(10)    VALUE '**'.
               10  FILLER PIC X(01)    VALUE 'H'.
               10  FILLER PIC X(01)    VALUE 'Y'.
           05  FILLER.
               10  FILLER PIC X(04)    VALUE 'AML3'.
               10  FILLER PIC X(08)    VALUE 'AMLPEPLG'.
               10  FILLER PIC X(40)    VALUE
                   'PEP LARGE TRANSACTION'.
               10  FILLER PIC 9(03)    VALUE 000.
               10  FILLER PIC 9(09)V99 VALUE 50000.00.
               10  FILLER PIC 9(03)    VALUE 001.
               10  FILLER PIC 9(03)    VALUE 000.
               10  FILLER PIC X(10)    VALUE '**'.
               10  FILLER PIC X(01)    VALUE 'H'.
               10  FILLER PIC X(01)    VALUE 'Y'.
           05  FILLER.
               10  FILLER PIC X(04)    VALUE 'AML4'.
               10  FILLER PIC X(08)    VALUE 'AMLMSBSP'.
               10  FILLER PIC X(40)    VALUE
                   'MSB CUSTOMER VOLUME SPIKE'.
               10  FILLER PIC 9(03)    VALUE 030.
               10  FILLER PIC 9(09)V99 VALUE 0.
               10  FILLER PIC 9(03)    VALUE 001.
               10  FILLER PIC 9(03)    VALUE 300.
               10  FILLER PIC X(10)    VALUE 'CBIN'.
               10  FILLER PIC X(01)    VALUE 'M'.
               10  FILLER PIC X(01)    VALUE 'Y'.
           05  FILLER.
               10  FILLER PIC X(04)    VALUE 'AML5'.
               10  FILLER PIC X(08)    VALUE 'AMLDORMR'.
               10  FILLER PIC X(40)    VALUE
                   'DORMANT ACCOUNT SUDDEN ACTIVITY'.
               10  FILLER PIC 9(03)    VALUE 030.
               10  FILLER PIC 9(09)V99 VALUE 5000.00.
               10  FILLER PIC 9(03)    VALUE 001.
               10  FILLER PIC 9(03)    VALUE 000.
               10  FILLER PIC X(10)    VALUE '**'.
               10  FILLER PIC X(01)    VALUE 'M'.
               10  FILLER PIC X(01)    VALUE 'Y'.
           05  FILLER.
               10  FILLER PIC X(04)    VALUE 'AML6'.
               10  FILLER PIC X(08)    VALUE 'AMLRNDWR'.
               10  FILLER PIC X(40)    VALUE
                   'ROUND-AMOUNT WIRE TO NEW BENEFICIARY'.
               10  FILLER PIC 9(03)    VALUE 030.
               10  FILLER PIC 9(09)V99 VALUE 100000.00.
               10  FILLER PIC 9(03)    VALUE 001.
               10  FILLER PIC 9(03)    VALUE 000.
               10  FILLER PIC X(10)    VALUE '**'.
               10  FILLER PIC X(01)    VALUE 'H'.
               10  FILLER PIC X(01)    VALUE 'Y'.
           05  FILLER.
               10  FILLER PIC X(04)    VALUE 'AML7'.
               10  FILLER PIC X(08)    VALUE 'AMLPROFL'.
               10  FILLER PIC X(40)    VALUE
                   'ACTIVITY INCONSISTENT WITH PROFILE'.
               10  FILLER PIC 9(03)    VALUE 030.
               10  FILLER PIC 9(09)V99 VALUE 0.
               10  FILLER PIC 9(03)    VALUE 001.
               10  FILLER PIC 9(03)    VALUE 200.
               10  FILLER PIC X(10)    VALUE 'RTMAPW'.
               10  FILLER PIC X(01)    VALUE 'M'.
               10  FILLER PIC X(01)    VALUE 'Y'.
       01  AML-SCENARIO-TABLE REDEFINES AML-SCENARIO-VALUES.
           05  SCEN-ENTRY OCCURS 7 TIMES INDEXED BY SCEN-IDX.
               10  SCEN-ID                 PIC X(04).
               10  SCEN-RULE-ID            PIC X(08).
               10  SCEN-DESCRIPTION        PIC X(40).
               10  SCEN-LOOKBACK-DAYS      PIC 9(03).
               10  SCEN-AMOUNT             PIC 9(09)V99.
               10  SCEN-COUNT              PIC 9(03).
               10  SCEN-PCT                PIC 9(03).
               10  SCEN-SEGMENTS           PIC X(10).
               10  SCEN-SEG REDEFINES SCEN-SEGMENTS
                                           PIC X(02) OCCURS 5.
               10  SCEN-SEVERITY           PIC X(01).
               10  SCEN-ACTIVE             PIC X(01).
       01  AML-SCENARIO-COUNT              PIC 9(02) VALUE 7.
      *
      *    ROUND AMOUNT UNIT FOR AML6 (WHOLE THOUSANDS)
       01  AML-ROUND-UNIT                  PIC 9(05) VALUE 1000.
      *
      *----------------------------------------------------------------*
      * HIGH-RISK GEOGRAPHY - ISO 3166 ALPHA-2                         *
      *   B = FATF CALL FOR ACTION / COMPREHENSIVELY SANCTIONED        *
      *   G = FATF INCREASED MONITORING (GREY LIST) - SAMPLE           *
      *----------------------------------------------------------------*
       01  AML-HIGH-RISK-COUNTRY-VALUES.
           05  FILLER PIC X(03) VALUE 'IRB'.
           05  FILLER PIC X(03) VALUE 'KPB'.
           05  FILLER PIC X(03) VALUE 'MMB'.
           05  FILLER PIC X(03) VALUE 'SYB'.
           05  FILLER PIC X(03) VALUE 'CUB'.
           05  FILLER PIC X(03) VALUE 'HTG'.
           05  FILLER PIC X(03) VALUE 'NGG'.
           05  FILLER PIC X(03) VALUE 'VNG'.
           05  FILLER PIC X(03) VALUE 'YEG'.
           05  FILLER PIC X(03) VALUE 'VEG'.
           05  FILLER PIC X(03) VALUE 'ZAG'.
           05  FILLER PIC X(03) VALUE 'KEG'.
           05  FILLER PIC X(03) VALUE 'LBG'.
           05  FILLER PIC X(03) VALUE 'SSG'.
           05  FILLER PIC X(03) VALUE 'MZG'.
           05  FILLER PIC X(03) VALUE 'CMG'.
       01  AML-HIGH-RISK-COUNTRY-TABLE
                         REDEFINES AML-HIGH-RISK-COUNTRY-VALUES.
           05  HRC-ENTRY OCCURS 16 TIMES INDEXED BY HRC-IDX.
               10  HRC-COUNTRY             PIC X(02).
               10  HRC-LEVEL               PIC X(01).
                   88  HRC-BLACK-LIST                VALUE 'B'.
                   88  HRC-GREY-LIST                 VALUE 'G'.
       01  AML-HIGH-RISK-COUNTRY-COUNT     PIC 9(02) VALUE 16.
