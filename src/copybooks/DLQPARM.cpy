      *================================================================*
      * COPYBOOK : DLQPARM                                             *
      * PURPOSE  : LINKAGE FOR DELQBKT - DELINQUENCY RULES             *
      *   AGE   BUCKET, STATUS, BUREAU, NON-ACCRUAL, CHARGE-OFF, ALERT *
      *   LFEE  LATE FEE FOR ONE INSTALLMENT                           *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-06 RJT  ORIGINAL                                       *
      * 2009-08-03 DPS  FFIEC UNIFORM RETAIL CREDIT CLASSIFICATION     *
      * 2016-04-25 AKW  LPER LATE FEE / STATE CAPS                     *
      *================================================================*
       01  DELQ-PARMS.
           05  DLQ-FUNCTION                PIC X(04).
               88  DLQ-AGE                           VALUE 'AGE '.
               88  DLQ-LATE-FEE                      VALUE 'LFEE'.
      *--- INPUT -----------------------------------------------------*
           05  DLQ-PRODUCT-CODE            PIC X(04).
           05  DLQ-CATEGORY                PIC X(02).
           05  DLQ-CURRENT-STATUS          PIC X(01).
           05  DLQ-DAYS-PAST-DUE           PIC 9(04).
           05  DLQ-PRIOR-DPD               PIC 9(04).
           05  DLQ-INSTALLMENT-AMT         PIC S9(11)V99 COMP-3.
           05  DLQ-INSTALL-DAYS-LATE       PIC 9(04).
           05  DLQ-FEE-ASSESSED-FLAG       PIC X(01).
           05  DLQ-STATE                   PIC X(02).
      *--- OUTPUT ----------------------------------------------------*
           05  DLQ-BUCKET                  PIC X(01).
           05  DLQ-BUCKET-DESC             PIC X(10).
           05  DLQ-NEW-STATUS              PIC X(01).
           05  DLQ-BUREAU-FLAG             PIC X(01).
           05  DLQ-NONACCRUAL-FLAG         PIC X(01).
           05  DLQ-CHARGEOFF-FLAG          PIC X(01).
           05  DLQ-ALERT-FLAG              PIC X(01).
           05  DLQ-ALERT-SEVERITY          PIC X(01).
           05  DLQ-CROSSED-30              PIC X(01).
           05  DLQ-CROSSED-60              PIC X(01).
           05  DLQ-CROSSED-90              PIC X(01).
           05  DLQ-ASSESS-FEE-FLAG         PIC X(01).
           05  DLQ-LATE-FEE-AMT            PIC S9(07)V99 COMP-3.
           05  DLQ-RETURN-CODE             PIC 9(02).
               88  DLQ-OK                            VALUE 00.
               88  DLQ-INVALID                       VALUE 12.
