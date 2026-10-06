      *================================================================*
      * COPYBOOK : ELIGPARM                                            *
      * PURPOSE  : LINKAGE FOR ELIGCHK - DEPOSIT PRODUCT ELIGIBILITY   *
      *            FOR ACCOUNT OPENING                                 *
      *   CALLER SUPPLIES PRODUCT ATTRIBUTES (FROM PRODMST), CUSTOMER  *
      *   ATTRIBUTES (FROM CUSTMST), OPENING DEPOSIT AND THE NUMBER OF *
      *   OPEN ACCOUNTS THE CUSTOMER ALREADY HOLDS IN THIS PRODUCT.    *
      *   ALL APPLICABLE DECLINE REASONS ARE RETURNED (NOT FIRST-FAIL).*
      *   DECLINE CODES:                                               *
      *     E01 PRODUCT IS NOT A DEPOSIT PRODUCT                       *
      *     E02 PRODUCT RETIRED OR NOT YET EFFECTIVE                   *
      *     E03 PRODUCT NOT OFFERED TO THIS CUSTOMER TYPE              *
      *     E04 CUSTOMER STATUS DOES NOT PERMIT NEW ACCOUNTS           *
      *     E05 KYC NOT VERIFIED (EXPIRED / FAILED / EDD OUTSTANDING)  *
      *     E06 KYC PENDING - PRODUCT OR AMOUNT NOT PERMITTED          *
      *     E07 OPENING DEPOSIT BELOW PRODUCT MINIMUM                  *
      *     E08 INDIVIDUAL UNDER AGE 18 (CUSTODIAL NOT SUPPORTED)      *
      *     E09 MAXIMUM NUMBER OF ACCOUNTS FOR PRODUCT REACHED         *
      *     E10 CORPORATE PRODUCT REQUIRES CB OR IN SEGMENT            *
      *     E11 OFAC SCREENING NOT CLEAR                               *
      *     E12 CUSTOMER RISK RATING PROHIBITED                        *
      *     E13 DATE OF BIRTH MISSING OR INVALID                       *
      *     E14 KYC PERIODIC REVIEW OVERDUE                            *
      *   RC : 00 ELIGIBLE, 08 DECLINED, 12 INVALID INPUT              *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2003-01-20 BHS  ORIGINAL                                       *
      * 2017-02-08 AKW  ONLINE SAVINGS (HYSV) RULES                    *
      * 2020-08-03 JLM  RETURN ALL DECLINE REASONS (CHG1960)           *
      *================================================================*
       01  ELIGCHK-PARMS.
           05  ELG-INPUT.
               10  ELG-PRODUCT-CODE        PIC X(04).
               10  ELG-PROD-CATEGORY       PIC X(02).
               10  ELG-PROD-CUST-TYPE      PIC X(01).
               10  ELG-PROD-STATUS         PIC X(01).
               10  ELG-PROD-EFF-DATE       PIC 9(08).
               10  ELG-PROD-MIN-OPEN-AMT   PIC S9(13)V99 COMP-3.
               10  ELG-CUST-TYPE           PIC X(01).
               10  ELG-CUST-STATUS         PIC X(01).
               10  ELG-KYC-STATUS          PIC X(01).
               10  ELG-KYC-REVIEW-DATE     PIC 9(08).
               10  ELG-BIRTH-DATE          PIC 9(08).
               10  ELG-SEGMENT             PIC X(02).
               10  ELG-OFAC-STATUS         PIC X(01).
               10  ELG-RISK-RATING         PIC X(01).
               10  ELG-OPEN-AMOUNT         PIC S9(13)V99 COMP-3.
               10  ELG-EXISTING-SAME-PROD  PIC 9(03).
               10  ELG-AS-OF-DATE          PIC 9(08).
           05  ELG-OUTPUT.
               10  ELG-ELIGIBLE-FLAG       PIC X(01).
                   88  ELG-IS-ELIGIBLE               VALUE 'Y'.
               10  ELG-CUST-AGE            PIC 9(03).
               10  ELG-DECLINE-COUNT       PIC 9(02).
               10  ELG-DECLINE-CODE        PIC X(03) OCCURS 10 TIMES.
           05  ELG-RETURN-CODE             PIC 9(02).
