      *================================================================*
      * COPYBOOK : NACHAREC                                            *
      * PURPOSE  : NACHA ACH FILE RECORD LAYOUTS (94 BYTES, BLOCKED 10)*
      *            ONE 94-BYTE AREA REDEFINED BY RECORD TYPE CODE:     *
      *              1 FILE HEADER          5 BATCH HEADER             *
      *              6 ENTRY DETAIL         7 ADDENDA (05 / 99 RETURN) *
      *              8 BATCH CONTROL        9 FILE CONTROL / PADDING   *
      * FILES    : ACHINBND (INBOUND FROM FED), ACHOUTB (ORIGINATED),  *
      *            ACHRTN (RETURN ENTRY STAGING), ACHRTNF (RETURNS)    *
      * NOTES    : AMOUNTS ARE UNSIGNED, IMPLIED 2 DECIMALS.           *
      *            ENTRY HASH = SUM OF 8-DIGIT RDFI ROUTING NUMBERS,   *
      *            RIGHTMOST 10 DIGITS (MOD 10**10).                   *
      *            ALL-9 RECORDS PAD THE LAST BLOCK - IGNORE ON INPUT. *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-02-12 RJT  ORIGINAL - PPD/CCD ORIGINATION                 *
      * 2001-04-30 BHS  RETURN ADDENDA (TYPE 99) FOR ACHRET            *
      * 2013-09-16 DPS  CTX / WEB                                      *
      * 2016-09-23 DPS  SAME-DAY ACH - COMPANY DESCRIPTIVE DATE 'SD'   *
      *================================================================*
       01  NACHA-RECORD.
           05  NACHA-REC-DATA              PIC X(94).
           05  NACHA-REC-TYPE-R REDEFINES NACHA-REC-DATA.
               10  NACHA-REC-TYPE          PIC X(01).
                   88  NACHA-FILE-HEADER             VALUE '1'.
                   88  NACHA-BATCH-HEADER            VALUE '5'.
                   88  NACHA-ENTRY-DETAIL            VALUE '6'.
                   88  NACHA-ADDENDA                 VALUE '7'.
                   88  NACHA-BATCH-CONTROL           VALUE '8'.
                   88  NACHA-FILE-CONTROL            VALUE '9'.
               10  FILLER                  PIC X(93).
      *--- 1 FILE HEADER ----------------------------------------------*
           05  NFH-RECORD REDEFINES NACHA-REC-DATA.
               10  NFH-REC-TYPE            PIC X(01).
               10  NFH-PRIORITY-CODE       PIC X(02).
      *            BLANK + 9 DIGIT ROUTING
               10  NFH-IMMED-DEST          PIC X(10).
               10  NFH-IMMED-ORIGIN        PIC X(10).
      *            YYMMDD / HHMM
               10  NFH-CREATION-DATE       PIC 9(06).
               10  NFH-CREATION-TIME       PIC 9(04).
               10  NFH-FILE-ID-MODIFIER    PIC X(01).
               10  NFH-RECORD-SIZE         PIC X(03).
               10  NFH-BLOCKING-FACTOR     PIC X(02).
               10  NFH-FORMAT-CODE         PIC X(01).
               10  NFH-IMMED-DEST-NAME     PIC X(23).
               10  NFH-IMMED-ORIGIN-NAME   PIC X(23).
               10  NFH-REFERENCE-CODE      PIC X(08).
      *--- 5 BATCH HEADER ---------------------------------------------*
           05  NBH-RECORD REDEFINES NACHA-REC-DATA.
               10  NBH-REC-TYPE            PIC X(01).
      *            200 MIXED, 220 CREDITS ONLY, 225 DEBITS ONLY
               10  NBH-SERVICE-CLASS       PIC 9(03).
               10  NBH-COMPANY-NAME        PIC X(16).
               10  NBH-COMPANY-DISC-DATA   PIC X(20).
               10  NBH-COMPANY-ID          PIC X(10).
               10  NBH-SEC-CODE            PIC X(03).
               10  NBH-ENTRY-DESC          PIC X(10).
      *            'SDHHMM' = SAME-DAY ENTRY REQUESTED
               10  NBH-DESCRIPTIVE-DATE    PIC X(06).
               10  NBH-EFFECTIVE-DATE      PIC X(06).
      *            JULIAN SETTLEMENT DATE - INSERTED BY ACH OPERATOR
               10  NBH-SETTLEMENT-DATE     PIC X(03).
               10  NBH-ORIGINATOR-STATUS   PIC X(01).
               10  NBH-ODFI-ID             PIC X(08).
               10  NBH-BATCH-NUMBER        PIC 9(07).
      *--- 6 ENTRY DETAIL ---------------------------------------------*
           05  NED-RECORD REDEFINES NACHA-REC-DATA.
               10  NED-REC-TYPE            PIC X(01).
      *            22 DDA CR  23 DDA CR PRENOTE  27 DDA DR  28 PRENOTE
      *            32 SAV CR  33 SAV CR PRENOTE  37 SAV DR  38 PRENOTE
      *            21/26/31/36 = RETURN OR NOC OF THE ABOVE
               10  NED-TRAN-CODE           PIC X(02).
               10  NED-RDFI-ID             PIC X(08).
               10  NED-CHECK-DIGIT         PIC X(01).
               10  NED-DFI-ACCOUNT         PIC X(17).
               10  NED-AMOUNT              PIC 9(08)V99.
               10  NED-INDIVIDUAL-ID       PIC X(15).
               10  NED-INDIVIDUAL-NAME     PIC X(22).
               10  NED-DISCRETIONARY       PIC X(02).
               10  NED-ADDENDA-IND         PIC X(01).
               10  NED-TRACE-NUMBER.
                   15  NED-TRACE-ODFI      PIC X(08).
                   15  NED-TRACE-SEQ       PIC 9(07).
      *--- 7 ADDENDA - TYPE 05 PAYMENT RELATED INFORMATION ------------*
           05  NAD-RECORD REDEFINES NACHA-REC-DATA.
               10  NAD-REC-TYPE            PIC X(01).
               10  NAD-ADDENDA-TYPE        PIC X(02).
               10  NAD-PAYMENT-INFO        PIC X(80).
               10  NAD-ADDENDA-SEQ         PIC 9(04).
               10  NAD-ENTRY-SEQ           PIC 9(07).
      *--- 7 ADDENDA - TYPE 99 RETURN ---------------------------------*
           05  NAR-RECORD REDEFINES NACHA-REC-DATA.
               10  NAR-REC-TYPE            PIC X(01).
               10  NAR-ADDENDA-TYPE        PIC X(02).
               10  NAR-RETURN-REASON       PIC X(03).
               10  NAR-ORIG-TRACE          PIC X(15).
               10  NAR-DATE-OF-DEATH       PIC X(06).
               10  NAR-ORIG-RDFI           PIC X(08).
      *            LOCAL USE (STAGING): SEC(3) CO-ID(10) CO-NAME(16)
      *            EFF-DATE(6) - CARRIED TO ACHRTNF BATCH HEADERS
               10  NAR-ADDENDA-INFO.
                   15  NAR-INFO-SEC        PIC X(03).
                   15  NAR-INFO-CO-ID      PIC X(10).
                   15  NAR-INFO-CO-NAME    PIC X(16).
                   15  NAR-INFO-EFF-DATE   PIC X(06).
                   15  FILLER              PIC X(09).
               10  NAR-TRACE-NUMBER        PIC X(15).
      *--- 8 BATCH CONTROL --------------------------------------------*
           05  NBC-RECORD REDEFINES NACHA-REC-DATA.
               10  NBC-REC-TYPE            PIC X(01).
               10  NBC-SERVICE-CLASS       PIC 9(03).
               10  NBC-ENTRY-ADDENDA-CNT   PIC 9(06).
               10  NBC-ENTRY-HASH          PIC 9(10).
               10  NBC-TOTAL-DEBIT         PIC 9(10)V99.
               10  NBC-TOTAL-CREDIT        PIC 9(10)V99.
               10  NBC-COMPANY-ID          PIC X(10).
               10  NBC-MAC                 PIC X(19).
               10  NBC-RESERVED            PIC X(06).
               10  NBC-ODFI-ID             PIC X(08).
               10  NBC-BATCH-NUMBER        PIC 9(07).
      *--- 9 FILE CONTROL ---------------------------------------------*
           05  NFC-RECORD REDEFINES NACHA-REC-DATA.
               10  NFC-REC-TYPE            PIC X(01).
               10  NFC-BATCH-COUNT         PIC 9(06).
               10  NFC-BLOCK-COUNT         PIC 9(06).
               10  NFC-ENTRY-ADDENDA-CNT   PIC 9(08).
               10  NFC-ENTRY-HASH          PIC 9(10).
               10  NFC-TOTAL-DEBIT         PIC 9(10)V99.
               10  NFC-TOTAL-CREDIT        PIC 9(10)V99.
               10  NFC-RESERVED            PIC X(39).
