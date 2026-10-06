      *================================================================*
      * COPYBOOK : YKPRCP                                              *
      * PURPOSE  : LINKAGE FOR DATE WINDOW BRIDGE RULE MODULE YKPRC    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-11-28 LMB  ORIGINAL                                       *
      * 2004-11-19 PQR  AUDIT FINDING - CONTROL CONTROL TOTALS CHG3404 *
      * 2004-09-28 RJT  CONTROL RERUN PROTECTION CHG7805               *
      * 2005-07-27 GAF  AMOUNT-1 TABLE EXPANDED REQ5683                *
      * 2008-09-08 RJT  AMOUNT-2 LIMIT REVISED PRB5015                 *
      * 2008-06-22 PQR  NEW AMOUNT-1 REPORT COLUMNS REQ8624            *
      *================================================================*
       01  YKPRC-PARMS.
           05  YKPRC-AMOUNT-1-AMT          PIC S9(13)V99 COMP-3.
           05  YKPRC-CONTROL-YTD           PIC S9(13)V99 COMP-3.
           05  YKPRC-AMOUNT-1-YTD          PIC S9(13)V99 COMP-3.
           05  YKPRC-AMOUNT-2-AMT          PIC S9(13)V99 COMP-3.
           05  YKPRC-CONTROL-AMT           PIC S9(13)V99 COMP-3.
           05  YKPRC-AMOUNT-2-YTD          PIC S9(13)V99 COMP-3.
           05  YKPRC-EXPIRY-DATE           PIC 9(08).
           05  YKPRC-LAST-STMT-DATE        PIC 9(08).
           05  YKPRC-MATURITY-DATE         PIC 9(08).
           05  YKPRC-LAST-ACT-DATE         PIC 9(08).
           05  YKPRC-CYCLE-CD              PIC X(02).
               88  YKPRC-CYCLE-CD-FJ               VALUE 'FJ'.
               88  YKPRC-CYCLE-CD-LA               VALUE 'LA'.
               88  YKPRC-CYCLE-CD-SP               VALUE 'SP'.
               88  YKPRC-CYCLE-CD-UC               VALUE 'UC'.
           05  YKPRC-SOURCE-CD             PIC X(03).
               88  YKPRC-SOURCE-CD-BNX             VALUE 'BNX'.
               88  YKPRC-SOURCE-CD-FTB             VALUE 'FTB'.
               88  YKPRC-SOURCE-CD-GAP             VALUE 'GAP'.
               88  YKPRC-SOURCE-CD-PAP             VALUE 'PAP'.
               88  YKPRC-SOURCE-CD-VGC             VALUE 'VGC'.
               88  YKPRC-SOURCE-CD-VJV             VALUE 'VJV'.
           05  YKPRC-PIVOT-CD              PIC X(03).
               88  YKPRC-PIVOT-CD-KGT              VALUE 'KGT'.
               88  YKPRC-PIVOT-CD-NNG              VALUE 'NNG'.
               88  YKPRC-PIVOT-CD-PTC              VALUE 'PTC'.
               88  YKPRC-PIVOT-CD-WZW              VALUE 'WZW'.
           05  YKPRC-REGION-CD             PIC X(01).
           05  YKPRC-NOTICE-SENT-FLAG      PIC X(01).
               88  YKPRC-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  YKPRC-AUTO-RENEW-FLAG       PIC X(01).
               88  YKPRC-AUTO-RENEW-FLAG-YES       VALUE 'Y'.
           05  YKPRC-PREFERRED-FLAG        PIC X(01).
               88  YKPRC-PREFERRED-FLAG-YES        VALUE 'Y'.
           05  YKPRC-ITEM-CNT              PIC 9(07).
           05  YKPRC-PAYMENT-CNT           PIC 9(07).
           05  YKPRC-MARGIN                PIC 9(03)V9(06).
           05  YKPRC-DISCOUNT              PIC 9(03)V9(06).
           05  YKPRC-NAME                  PIC X(30).
           05  YKPRC-CONTACT               PIC X(20).
           05  YKPRC-RETURN-CODE           PIC 9(02).
