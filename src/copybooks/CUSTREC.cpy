      *================================================================*
      * COPYBOOK : CUSTREC                                             *
      * PURPOSE  : CUSTOMER INFORMATION FILE (CIF) MASTER RECORD       *
      * FILE     : CUSTMST  (KSDS, KEY = CUST-ID)                      *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1988-07-01 RJT  ORIGINAL                                       *
      * 1998-11-02 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 2002-03-15 BHS  USA PATRIOT ACT - KYC STATUS, RISK RATING      *
      * 2006-08-30 BHS  PEP FLAG, OFAC SCREENING FIELDS                *
      * 2011-04-12 DPS  CORPORATE HIERARCHY (PARENT CUST ID)           *
      * 2018-05-11 AKW  FINCEN CDD RULE - KYC TIER, REVIEW DATE        *
      * 2019-10-07 AKW  CTR EXEMPTION / MSB FLAGS                      *
      *================================================================*
       01  CUSTOMER-RECORD.
           05  CUST-ID                     PIC X(10).
           05  CUST-TYPE                   PIC X(01).
               88  CUST-INDIVIDUAL                   VALUE 'I'.
               88  CUST-CORPORATE                    VALUE 'C'.
               88  CUST-TRUST                        VALUE 'T'.
           05  CUST-NAME.
      *        FOR CORPORATE/TRUST, LAST-NAME HOLDS THE LEGAL NAME
               10  CUST-LAST-NAME          PIC X(30).
               10  CUST-FIRST-NAME         PIC X(20).
               10  CUST-MIDDLE-INIT        PIC X(01).
           05  CUST-TAX-ID                 PIC X(09).
           05  CUST-TAX-ID-TYPE            PIC X(01).
               88  CUST-TAXID-SSN                    VALUE 'S'.
               88  CUST-TAXID-EIN                    VALUE 'E'.
               88  CUST-TAXID-ITIN                   VALUE 'I'.
               88  CUST-TAXID-NONE                   VALUE 'N'.
      *    DATE OF BIRTH (INDIVIDUAL) OR INCORPORATION DATE (ENTITY)
           05  CUST-BIRTH-DATE             PIC 9(08).
           05  CUST-ADDRESS.
               10  CUST-ADDR-LINE1         PIC X(35).
               10  CUST-ADDR-LINE2         PIC X(35).
               10  CUST-CITY               PIC X(25).
               10  CUST-STATE              PIC X(02).
               10  CUST-ZIP                PIC X(10).
               10  CUST-COUNTRY            PIC X(02).
           05  CUST-PHONE                  PIC X(15).
           05  CUST-EMAIL                  PIC X(50).
           05  CUST-SINCE-DATE             PIC 9(08).
           05  CUST-STATUS                 PIC X(01).
               88  CUST-ACTIVE                       VALUE 'A'.
               88  CUST-INACTIVE                     VALUE 'I'.
               88  CUST-DECEASED                     VALUE 'D'.
               88  CUST-BLOCKED                      VALUE 'B'.
               88  CUST-PROSPECT                     VALUE 'P'.
           05  CUST-KYC-STATUS             PIC X(01).
               88  CUST-KYC-VERIFIED                 VALUE 'V'.
               88  CUST-KYC-PENDING                  VALUE 'P'.
               88  CUST-KYC-FAILED                   VALUE 'F'.
               88  CUST-KYC-EXPIRED                  VALUE 'E'.
               88  CUST-KYC-EDD-REQUIRED             VALUE 'R'.
           05  CUST-KYC-TIER               PIC X(01).
               88  CUST-KYC-TIER-SIMPLIFIED          VALUE '1'.
               88  CUST-KYC-TIER-STANDARD            VALUE '2'.
               88  CUST-KYC-TIER-ENHANCED            VALUE '3'.
           05  CUST-KYC-REVIEW-DATE        PIC 9(08).
           05  CUST-RISK-RATING            PIC X(01).
               88  CUST-RISK-LOW                     VALUE 'L'.
               88  CUST-RISK-MEDIUM                  VALUE 'M'.
               88  CUST-RISK-HIGH                    VALUE 'H'.
               88  CUST-RISK-PROHIBITED              VALUE 'P'.
           05  CUST-RISK-SCORE             PIC 9(03).
           05  CUST-PEP-FLAG               PIC X(01).
               88  CUST-IS-PEP                       VALUE 'Y'.
      *    NAICS CODE FOR ENTITIES, OCCUPATION CODE FOR INDIVIDUALS
           05  CUST-INDUSTRY-CODE          PIC X(06).
      *    ANNUAL INCOME (INDIVIDUAL) OR ANNUAL REVENUE (ENTITY)
           05  CUST-ANNUAL-INCOME          PIC S9(13)V99 COMP-3.
           05  CUST-SEGMENT                PIC X(02).
               88  CUST-SEG-RETAIL                   VALUE 'RT'.
               88  CUST-SEG-MASS-AFFLUENT            VALUE 'MA'.
               88  CUST-SEG-PRIVATE-WEALTH           VALUE 'PW'.
               88  CUST-SEG-CORPORATE                VALUE 'CB'.
               88  CUST-SEG-INSTITUTIONAL            VALUE 'IN'.
           05  CUST-RELATIONSHIP-ID        PIC X(10).
           05  CUST-PARENT-CUST-ID         PIC X(10).
           05  CUST-EMPLOYEE-FLAG          PIC X(01).
               88  CUST-IS-EMPLOYEE                  VALUE 'Y'.
           05  CUST-OFAC-STATUS            PIC X(01).
               88  CUST-OFAC-CLEAR                   VALUE 'C'.
               88  CUST-OFAC-POTENTIAL-HIT           VALUE 'H'.
               88  CUST-OFAC-CONFIRMED-HIT           VALUE 'R'.
           05  CUST-OFAC-LAST-SCREEN       PIC 9(08).
           05  CUST-CTR-EXEMPT-FLAG        PIC X(01).
               88  CUST-CTR-EXEMPT                   VALUE 'Y'.
           05  CUST-MSB-FLAG               PIC X(01).
               88  CUST-IS-MSB                       VALUE 'Y'.
      *    SUM OF ALL DEPOSIT BALANCES IN RELATIONSHIP - SET BY RELBAL
           05  CUST-TOTAL-REL-BALANCE      PIC S9(13)V99 COMP-3.
           05  CUST-LAST-MAINT-DATE        PIC 9(08).
           05  CUST-LAST-MAINT-USER        PIC X(08).
           05  FILLER                      PIC X(50).
