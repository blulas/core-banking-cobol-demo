      *================================================================*
      * COPYBOOK : RTTPARM                                             *
      * PURPOSE  : LINKAGE FOR RATETIER - DEPOSIT RATE / APY SELECTION *
      *   CALLER SUPPLIES PRODUCT, BALANCE, RELATIONSHIP BALANCE,      *
      *   SEGMENT, TERM (CDS), AS-OF DATE, PROMO RATE/EXPIRY AND THE   *
      *   EMPLOYEE FLAG. RATETIER RETURNS THE ANNUAL RATE (PERCENT),   *
      *   THE DISCLOSED APY (PERCENT) AND THE TIER THAT WAS USED.      *
      *   RTT-TIER-ID VALUES: Tnnn TABLE TIER, PRMO PROMOTIONAL RATE,  *
      *                       IDX  INDEX-PRICED (CMM1), NONE NOT       *
      *                       INTEREST BEARING                         *
      *   RETURN CODES: 00 OK                                          *
      *                 04 OK - PROMO EXPIRED / RATE CAPPED / FLOORED  *
      *                 08 PRODUCT NOT INTEREST BEARING (RATE ZERO)    *
      *                 12 PRODUCT OR RATE TABLE NOT FOUND             *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL                                       *
      * 2017-02-08 AKW  PROMO RATE / EXPIRY FOR ONLINE SAVINGS         *
      * 2019-08-19 AKW  RELATIONSHIP BALANCE AND SEGMENT (PW PRICING)  *
      * 2021-03-02 AKW  EMPLOYEE FLAG - CHG1904                        *
      *================================================================*
       01  RATETIER-PARMS.
           05  RTT-PRODUCT-CODE            PIC X(04).
           05  RTT-BALANCE                 PIC S9(13)V99 COMP-3.
           05  RTT-REL-BALANCE             PIC S9(13)V99 COMP-3.
           05  RTT-CUST-SEGMENT            PIC X(02).
           05  RTT-TERM-MONTHS             PIC 9(03).
           05  RTT-AS-OF-DATE              PIC 9(08).
           05  RTT-PROMO-RATE              PIC 9(03)V9(06).
           05  RTT-PROMO-EXP-DATE          PIC 9(08).
           05  RTT-EMPLOYEE-FLAG           PIC X(01).
           05  RTT-RESULT-RATE             PIC 9(03)V9(06).
           05  RTT-RESULT-APY              PIC 9(03)V9(06).
           05  RTT-TIER-ID                 PIC X(04).
           05  RTT-RETURN-CODE             PIC 9(02).
