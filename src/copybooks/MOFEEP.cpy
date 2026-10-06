      *================================================================*
      * COPYBOOK : MOFEEP                                              *
      * PURPOSE  : LINKAGE FOR MONEY ORDERS RULE MODULE MOFEE          *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-03-13 DPS  ORIGINAL                                       *
      * 1994-03-15 DPS  FEE EXCEPTIONS TO OPS QUEUE PRB2952            *
      * 1996-05-08 DPS  PERFORMANCE - VOIDED READ AHEAD INC1780        *
      * 1998-08-10 MKL  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-03-25 DPS  PERFORMANCE - PAID READ AHEAD PRB2226          *
      * 2001-11-23 BHS  OUTSTAND NOTICE TEXT PER LEGAL INC6021         *
      * 2003-05-16 CRP  ESCHEAT FEE SCHEDULE UPDATE INC8846            *
      * 2006-02-21 JLM  OUTSTAND LIMIT REVISED CHG6716                 *
      *================================================================*
       01  MOFEE-PARMS.
           05  MOFEE-OUTSTAND-AMT          PIC S9(13)V99 COMP-3.
           05  MOFEE-ESCHEAT-AMT           PIC S9(13)V99 COMP-3.
           05  MOFEE-PAID-YTD              PIC S9(13)V99 COMP-3.
           05  MOFEE-PAID-AMT              PIC S9(13)V99 COMP-3.
           05  MOFEE-FACE-AMT              PIC S9(13)V99 COMP-3.
           05  MOFEE-FEE-AMT               PIC S9(13)V99 COMP-3.
           05  MOFEE-LAST-STMT-DATE        PIC 9(08).
           05  MOFEE-LAST-PAY-DATE         PIC 9(08).
           05  MOFEE-LAST-ACT-DATE         PIC 9(08).
           05  MOFEE-EFFECTIVE-DATE        PIC 9(08).
           05  MOFEE-PAYEE-TYPE-CD         PIC X(02).
               88  MOFEE-PAYEE-TYPE-CD-CJ          VALUE 'CJ'.
               88  MOFEE-PAYEE-TYPE-CD-FY          VALUE 'FY'.
               88  MOFEE-PAYEE-TYPE-CD-HX          VALUE 'HX'.
               88  MOFEE-PAYEE-TYPE-CD-UV          VALUE 'UV'.
               88  MOFEE-PAYEE-TYPE-CD-VV          VALUE 'VV'.
           05  MOFEE-CHANNEL-CD            PIC X(02).
               88  MOFEE-CHANNEL-CD-DR             VALUE 'DR'.
               88  MOFEE-CHANNEL-CD-DX             VALUE 'DX'.
               88  MOFEE-CHANNEL-CD-PT             VALUE 'PT'.
               88  MOFEE-CHANNEL-CD-RN             VALUE 'RN'.
               88  MOFEE-CHANNEL-CD-YD             VALUE 'YD'.
               88  MOFEE-CHANNEL-CD-YU             VALUE 'YU'.
           05  MOFEE-REASON-CD             PIC X(01).
               88  MOFEE-REASON-CD-B               VALUE 'B'.
               88  MOFEE-REASON-CD-L               VALUE 'L'.
               88  MOFEE-REASON-CD-P               VALUE 'P'.
               88  MOFEE-REASON-CD-S               VALUE 'S'.
               88  MOFEE-REASON-CD-T               VALUE 'T'.
               88  MOFEE-REASON-CD-Z               VALUE 'Z'.
           05  MOFEE-ITEM-TYPE-CD          PIC X(01).
               88  MOFEE-ITEM-TYPE-CD-B            VALUE 'B'.
               88  MOFEE-ITEM-TYPE-CD-H            VALUE 'H'.
               88  MOFEE-ITEM-TYPE-CD-S            VALUE 'S'.
               88  MOFEE-ITEM-TYPE-CD-T            VALUE 'T'.
           05  MOFEE-OVERRIDE-FLAG         PIC X(01).
               88  MOFEE-OVERRIDE-FLAG-YES         VALUE 'Y'.
           05  MOFEE-STOP-FLAG             PIC X(01).
               88  MOFEE-STOP-FLAG-YES             VALUE 'Y'.
           05  MOFEE-PREFERRED-FLAG        PIC X(01).
               88  MOFEE-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  MOFEE-ITEM-CNT              PIC 9(07).
           05  MOFEE-RETRY-CNT             PIC 9(07).
           05  MOFEE-DISCOUNT              PIC 9(03)V9(06).
           05  MOFEE-FEE-PCT               PIC 9(03)V9(06).
           05  MOFEE-REF-NO                PIC X(16).
           05  MOFEE-NAME                  PIC X(30).
           05  MOFEE-RETURN-CODE           PIC 9(02).
