      *================================================================*
      * COPYBOOK : RATETBL                                             *
      * PURPOSE  : DEPOSIT INTEREST RATE TABLES AND PRICING CONSTANTS  *
      *            USED BY RATETIER.                                   *
      *            *** RATES ARE CHANGED BY PROGRAM CHANGE ONLY ***    *
      *            ALCO RATE MEMO -> CHG TICKET -> RECOMPILE RATETIER  *
      *            AND EVERY PROGRAM THAT LINKS IT STATICALLY.         *
      *   PART 1 - PRODUCT TO RATE TABLE MAP (COMPOUNDING FOR APY,     *
      *            BONUS ELIGIBILITY)                                  *
      *   PART 2 - RATE TIERS.  TYPE B = BALANCE TIER (LOW BOUND),     *
      *                         TYPE T = TERM TIER (MONTHS),           *
      *                         TYPE X = INDEX (RATE FIELD = SPREAD)   *
      *            TIERS WITHIN A TABLE MUST BE IN ASCENDING ORDER.    *
      *   PART 3 - RELATIONSHIP / EMPLOYEE BONUS, CAP AND FLOOR        *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL - SVS1, MMK1, CDT1                    *
      * 1999-01-04 MKL  MMK1 JUMBO TIER $1,000,000                     *
      * 2001-10-01 BHS  CMM1 INSTITUTIONAL MONEY MARKET                *
      * 2005-09-12 BHS  PRM1 PREMIER CHECKING                          *
      * 2017-02-08 AKW  HYS1 ONLINE SAVINGS, CDNP NO-PENALTY CD        *
      * 2019-08-19 AKW  RELATIONSHIP BONUS - CHG1655                   *
      * 2021-03-02 AKW  EMPLOYEE BONUS ON HYSV - CHG1904               *
      * 2023-02-06 JQP  CMM1 MOVED TO FED FUNDS INDEX PRICING - CHG2210*
      * 2025-12-11 JQP  ALCO 2025-12 RATE MEMO - CHG2797               *
      * 2026-07-30 JQP  ALCO 2026-07 RATE MEMO - CHG2851               *
      *================================================================*
      *--- PART 1 - PRODUCT MAP --------------------------------------*
      *    PROD(4) TABLE(4) COMPOUND(1) REL-BONUS(1) EMP-BONUS(1)      *
      *    TIME-DEPOSIT(1)                                             *
       01  RT-PRODUCT-MAP-VALUES.
           05  FILLER  PIC X(12) VALUE 'HYSVHYS1DYYN'.
           05  FILLER  PIC X(12) VALUE 'SVSTSVS1MNNN'.
           05  FILLER  PIC X(12) VALUE 'MMKTMMK1DYNN'.
           05  FILLER  PIC X(12) VALUE 'DDPRPRM1DNNN'.
           05  FILLER  PIC X(12) VALUE 'CD06CDT1DNNY'.
           05  FILLER  PIC X(12) VALUE 'CD12CDT1DNNY'.
           05  FILLER  PIC X(12) VALUE 'CD36CDT1DNNY'.
           05  FILLER  PIC X(12) VALUE 'CDNPCDNPDNNY'.
           05  FILLER  PIC X(12) VALUE 'CMMICMM1DNNN'.
           05  FILLER  PIC X(12) VALUE 'DDAPNONENNNN'.
           05  FILLER  PIC X(12) VALUE 'COPRNONENNNN'.
           05  FILLER  PIC X(12) VALUE 'CZBANONENNNN'.
           05  FILLER  PIC X(12) VALUE 'CVIRNONENNNN'.
       01  RT-PRODUCT-MAP REDEFINES RT-PRODUCT-MAP-VALUES.
           05  RT-PMAP-ENTRY OCCURS 13 TIMES INDEXED BY RT-PM-IDX.
               10  RT-PMAP-PRODUCT         PIC X(04).
               10  RT-PMAP-TABLE-ID        PIC X(04).
               10  RT-PMAP-COMPOUND        PIC X(01).
                   88  RT-PMAP-CMP-DAILY             VALUE 'D'.
                   88  RT-PMAP-CMP-MONTHLY           VALUE 'M'.
               10  RT-PMAP-REL-BONUS       PIC X(01).
               10  RT-PMAP-EMP-BONUS       PIC X(01).
               10  RT-PMAP-TIME-DEP        PIC X(01).
       01  RT-PRODUCT-MAP-COUNT            PIC 9(03) VALUE 13.

      *--- PART 2 - RATE TIERS ---------------------------------------*
      *    TABLE(4) TIER(4) TYPE(1) / LOW-BAL 9(11)V99 / TERM 9(3) /   *
      *    RATE 9(3)V9(6)                                              *
       01  RT-TIER-VALUES.
      *    HYS1 - ONLINE HIGH YIELD SAVINGS (FLAT - TIER 2 RESERVED    *
      *    FOR FUTURE BALANCE-BASED PRICING PER CHG1201)               *
           05  FILLER  PIC X(09)       VALUE 'HYS1T001B'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 4.100000.
           05  FILLER  PIC X(09)       VALUE 'HYS1T002B'.
           05  FILLER  PIC 9(11)V99    VALUE 10000.00.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 4.100000.
      *    SVS1 - STANDARD (PASSBOOK) SAVINGS - RETIRED PRODUCT        *
           05  FILLER  PIC X(09)       VALUE 'SVS1T001B'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 0.050000.
      *    MMK1 - MONEY MARKET                                         *
           05  FILLER  PIC X(09)       VALUE 'MMK1T001B'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 0.100000.
           05  FILLER  PIC X(09)       VALUE 'MMK1T002B'.
           05  FILLER  PIC 9(11)V99    VALUE 2500.00.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 2.500000.
           05  FILLER  PIC X(09)       VALUE 'MMK1T003B'.
           05  FILLER  PIC 9(11)V99    VALUE 25000.00.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 3.250000.
           05  FILLER  PIC X(09)       VALUE 'MMK1T004B'.
           05  FILLER  PIC 9(11)V99    VALUE 100000.00.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 3.600000.
           05  FILLER  PIC X(09)       VALUE 'MMK1T005B'.
           05  FILLER  PIC 9(11)V99    VALUE 1000000.00.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 3.850000.
      *    PRM1 - PREMIER CHECKING                                     *
           05  FILLER  PIC X(09)       VALUE 'PRM1T001B'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 0.050000.
           05  FILLER  PIC X(09)       VALUE 'PRM1T002B'.
           05  FILLER  PIC 9(11)V99    VALUE 25000.00.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 0.250000.
      *    CDT1 - CERTIFICATES OF DEPOSIT BY TERM. A TERM NOT IN THE   *
      *    TABLE TAKES THE RATE OF THE NEAREST SHORTER TERM.           *
           05  FILLER  PIC X(09)       VALUE 'CDT1T006T'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 6.
           05  FILLER  PIC 9(03)V9(06) VALUE 4.400000.
           05  FILLER  PIC X(09)       VALUE 'CDT1T012T'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 12.
           05  FILLER  PIC 9(03)V9(06) VALUE 4.650000.
           05  FILLER  PIC X(09)       VALUE 'CDT1T036T'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 36.
           05  FILLER  PIC 9(03)V9(06) VALUE 4.250000.
      *    CDNP - 11 MONTH NO-PENALTY CD                               *
           05  FILLER  PIC X(09)       VALUE 'CDNPT011T'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 11.
           05  FILLER  PIC 9(03)V9(06) VALUE 4.000000.
      *    CMM1 - INSTITUTIONAL MONEY MARKET - FED FUNDS MINUS SPREAD  *
      *    (BALANCE TIERS BELOW RETAINED FOR REFERENCE - CHG2210)      *
           05  FILLER  PIC X(09)       VALUE 'CMM1IDX X'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 0.280000.
           05  FILLER  PIC X(09)       VALUE 'CMB1T001B'.
           05  FILLER  PIC 9(11)V99    VALUE 0.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 3.900000.
           05  FILLER  PIC X(09)       VALUE 'CMB1T002B'.
           05  FILLER  PIC 9(11)V99    VALUE 5000000.00.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 4.000000.
           05  FILLER  PIC X(09)       VALUE 'CMB1T003B'.
           05  FILLER  PIC 9(11)V99    VALUE 25000000.00.
           05  FILLER  PIC 9(03)       VALUE 0.
           05  FILLER  PIC 9(03)V9(06) VALUE 4.050000.
       01  RT-TIER-TABLE REDEFINES RT-TIER-VALUES.
           05  RT-TIER-ENTRY OCCURS 18 TIMES INDEXED BY RT-TR-IDX.
               10  RT-TIER-TABLE-ID        PIC X(04).
               10  RT-TIER-ID              PIC X(04).
               10  RT-TIER-TYPE            PIC X(01).
                   88  RT-TIER-BY-BALANCE            VALUE 'B'.
                   88  RT-TIER-BY-TERM               VALUE 'T'.
                   88  RT-TIER-BY-INDEX              VALUE 'X'.
               10  RT-TIER-LOW-BAL         PIC 9(11)V99.
               10  RT-TIER-TERM            PIC 9(03).
               10  RT-TIER-RATE            PIC 9(03)V9(06).
       01  RT-TIER-COUNT                   PIC 9(03) VALUE 18.

      *--- PART 3 - BONUS, CAP AND FLOOR -----------------------------*
       01  RT-PRICING-CONSTANTS.
      *    RELATIONSHIP BONUS (CHG1655) - HYSV / MMKT ONLY
           05  RT-REL-BONUS-MIN-BAL        PIC 9(11)V99
                                           VALUE 250000.00.
           05  RT-REL-BONUS-RATE           PIC 9(03)V9(06)
                                           VALUE 0.100000.
      *    PRIVATE WEALTH SEGMENT GETS THE ENHANCED BONUS INSTEAD
           05  RT-PW-BONUS-RATE            PIC 9(03)V9(06)
                                           VALUE 0.150000.
      *    EMPLOYEE BONUS (CHG1904) - HYSV ONLY
           05  RT-EMP-BONUS-RATE           PIC 9(03)V9(06)
                                           VALUE 0.250000.
      *    NO NON-CD DEPOSIT RATE MAY EXCEED THE ALCO CAP
           05  RT-MAX-DEPOSIT-RATE         PIC 9(03)V9(06)
                                           VALUE 5.250000.
      *    CMM1 INDEX PRICING FLOOR (CHG2210)
           05  RT-INDEX-FLOOR-RATE         PIC 9(03)V9(06)
                                           VALUE 0.500000.
