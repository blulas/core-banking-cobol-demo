      *================================================================*
      * COPYBOOK : CRDPARM                                             *
      * PURPOSE  : LINKAGE FOR CRDECIDE - CREDIT DECISION              *
      *   CRD-APPL-TYPE 'C' CONSUMER (LPER)  'B' BUSINESS (COMMERCIAL) *
      *   DECISION APPROVE / REFER / DECLINE WITH UP TO 4 PRINCIPAL    *
      *   REASONS (REG B / ECOA ADVERSE ACTION) IN PRIORITY ORDER.     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 2016-04-25 AKW  ORIGINAL - LPER LAUNCH                         *
      * 2017-09-11 AKW  COUNTEROFFER AMOUNT - CHG2231                  *
      * 2019-06-03 JPL  BUSINESS DECISION RULES MOVED FROM CLS         *
      *================================================================*
       01  CREDIT-DECISION-PARMS.
           05  CRD-APPL-TYPE               PIC X(01).
               88  CRD-CONSUMER                      VALUE 'C'.
               88  CRD-BUSINESS                      VALUE 'B'.
      *--- CONSUMER INPUT --------------------------------------------*
           05  CRD-CREDIT-SCORE            PIC 9(03).
           05  CRD-ANNUAL-INCOME           PIC S9(11)V99 COMP-3.
           05  CRD-MONTHLY-DEBT            PIC S9(09)V99 COMP-3.
           05  CRD-REQUESTED-AMT           PIC S9(13)V99 COMP-3.
           05  CRD-TERM-MONTHS             PIC 9(03).
      *    RATE FROM LNPRICE - USED FOR THE NEW PAYMENT IN DTI
           05  CRD-PROPOSED-RATE           PIC 9(03)V9(06).
           05  CRD-EMPLOYMENT-MONTHS       PIC 9(03).
           05  CRD-BANKRUPTCY-7YR          PIC X(01).
           05  CRD-EXISTING-CUST           PIC X(01).
           05  CRD-KYC-STATUS              PIC X(01).
           05  CRD-STATE                   PIC X(02).
      *--- BUSINESS INPUT --------------------------------------------*
           05  CRD-PRODUCT-CODE            PIC X(04).
           05  CRD-RISK-GRADE              PIC 9(02).
           05  CRD-DSCR                    PIC 9(02)V99.
           05  CRD-LTV-PCT                 PIC 9(03)V99.
      *--- OUTPUT ----------------------------------------------------*
           05  CRD-DECISION                PIC X(07).
               88  CRD-APPROVED                      VALUE 'APPROVE'.
               88  CRD-REFERRED                      VALUE 'REFER  '.
               88  CRD-DECLINED                      VALUE 'DECLINE'.
           05  CRD-NEW-PAYMENT             PIC S9(11)V99 COMP-3.
           05  CRD-DTI-PCT                 PIC 9(03)V99.
           05  CRD-COUNTER-AMT             PIC S9(13)V99 COMP-3.
           05  CRD-REASON-COUNT            PIC 9(01).
           05  CRD-REASON                  OCCURS 4 TIMES.
               10  CRD-REASON-CODE         PIC X(03).
               10  CRD-REASON-TEXT         PIC X(40).
           05  CRD-RETURN-CODE             PIC 9(02).
               88  CRD-RC-APPROVE                    VALUE 00.
               88  CRD-RC-REFER                      VALUE 04.
               88  CRD-RC-DECLINE                    VALUE 08.
               88  CRD-RC-INVALID                    VALUE 12.
