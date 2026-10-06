      *================================================================*
      * COPYBOOK : CALTBL                                              *
      * PURPOSE  : FEDERAL RESERVE HOLIDAY CALENDAR. A DATE IN THIS    *
      *            TABLE, OR ANY SATURDAY/SUNDAY, IS A NON-BUSINESS    *
      *            DAY. SATURDAY HOLIDAYS ARE NOT OBSERVED BY THE FED. *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1994-12-01 RJT  ORIGINAL - TABLE MAINTAINED ANNUALLY BY OPS    *
      * 2021-06-18 AKW  ADDED JUNETEENTH                               *
      * 2024-12-02 OPS  LOADED 2025-2027                               *
      *    NOTE: TABLE MUST BE EXTENDED BEFORE 2028 YEAR END           *
      *================================================================*
       01  HOLIDAY-TABLE-VALUES.
      *    2025
           05  FILLER  PIC X(08) VALUE '20250101'.
           05  FILLER  PIC X(08) VALUE '20250120'.
           05  FILLER  PIC X(08) VALUE '20250217'.
           05  FILLER  PIC X(08) VALUE '20250526'.
           05  FILLER  PIC X(08) VALUE '20250619'.
           05  FILLER  PIC X(08) VALUE '20250704'.
           05  FILLER  PIC X(08) VALUE '20250901'.
           05  FILLER  PIC X(08) VALUE '20251013'.
           05  FILLER  PIC X(08) VALUE '20251111'.
           05  FILLER  PIC X(08) VALUE '20251127'.
           05  FILLER  PIC X(08) VALUE '20251225'.
      *    2026  (JUL 4 FALLS ON SATURDAY - NOT OBSERVED)
           05  FILLER  PIC X(08) VALUE '20260101'.
           05  FILLER  PIC X(08) VALUE '20260119'.
           05  FILLER  PIC X(08) VALUE '20260216'.
           05  FILLER  PIC X(08) VALUE '20260525'.
           05  FILLER  PIC X(08) VALUE '20260619'.
           05  FILLER  PIC X(08) VALUE '20260907'.
           05  FILLER  PIC X(08) VALUE '20261012'.
           05  FILLER  PIC X(08) VALUE '20261111'.
           05  FILLER  PIC X(08) VALUE '20261126'.
           05  FILLER  PIC X(08) VALUE '20261225'.
      *    2027  (JUN 19 AND DEC 25 FALL ON SATURDAY - NOT OBSERVED)
           05  FILLER  PIC X(08) VALUE '20270101'.
           05  FILLER  PIC X(08) VALUE '20270118'.
           05  FILLER  PIC X(08) VALUE '20270215'.
           05  FILLER  PIC X(08) VALUE '20270531'.
           05  FILLER  PIC X(08) VALUE '20270705'.
           05  FILLER  PIC X(08) VALUE '20270906'.
           05  FILLER  PIC X(08) VALUE '20271011'.
           05  FILLER  PIC X(08) VALUE '20271111'.
           05  FILLER  PIC X(08) VALUE '20271125'.
       01  HOLIDAY-TABLE REDEFINES HOLIDAY-TABLE-VALUES.
           05  HOLIDAY-ENTRY       PIC 9(08) OCCURS 30 TIMES
                                   INDEXED BY HOL-IDX.
       01  HOLIDAY-TABLE-COUNT     PIC 9(03) VALUE 30.
