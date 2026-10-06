      *================================================================*
      * COPYBOOK : KYCTPARM                                            *
      * PURPOSE  : LINKAGE FOR KYCTIER - KYC TIER / EDD DETERMINATION  *
      *   INPUT  : CUSTOMER PROFILE ATTRIBUTES + AS-OF DATE (THE DATE  *
      *            THE REVIEW IS COMPLETED / THE TIER IS ASSIGNED)     *
      *   OUTPUT : TIER 1 SIMPLIFIED / 2 STANDARD / 3 ENHANCED,        *
      *            EDD-REQUIRED FLAG, REVIEW CYCLE AND NEXT REVIEW DATE*
      *   RC     : 00 OK, 08 PROHIBITED RELATIONSHIP (CONFIRMED OFAC), *
      *            12 INVALID INPUT                                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2018-05-11 AKW  ORIGINAL - FINCEN CDD RULE                     *
      * 2019-10-07 AKW  MSB FLAG                                       *
      * 2022-06-15 JLM  REASON CODE OUTPUT (CHG2391)                   *
      *================================================================*
       01  KYCTIER-PARMS.
           05  KYT-INPUT.
               10  KYT-CUST-TYPE           PIC X(01).
               10  KYT-PEP-FLAG            PIC X(01).
               10  KYT-MSB-FLAG            PIC X(01).
               10  KYT-RISK-RATING         PIC X(01).
               10  KYT-COUNTRY             PIC X(02).
               10  KYT-SEGMENT             PIC X(02).
               10  KYT-ANNUAL-INCOME       PIC S9(13)V99 COMP-3.
               10  KYT-INDUSTRY-CODE       PIC X(06).
               10  KYT-OFAC-STATUS         PIC X(01).
               10  KYT-AS-OF-DATE          PIC 9(08).
           05  KYT-OUTPUT.
               10  KYT-RESULT-TIER         PIC X(01).
                   88  KYT-TIER-SIMPLIFIED           VALUE '1'.
                   88  KYT-TIER-STANDARD             VALUE '2'.
                   88  KYT-TIER-ENHANCED             VALUE '3'.
               10  KYT-EDD-REQUIRED        PIC X(01).
                   88  KYT-EDD-IS-REQUIRED           VALUE 'Y'.
               10  KYT-REVIEW-MONTHS       PIC 9(03).
               10  KYT-NEXT-REVIEW-DATE    PIC 9(08).
      *        PEPX MSBX HRSK OFAC CNTY INDH TRST PWLT CASH FRGN
      *        GOVT LOWI STDX
               10  KYT-REASON-CODE         PIC X(04).
           05  KYT-RETURN-CODE             PIC 9(02).
