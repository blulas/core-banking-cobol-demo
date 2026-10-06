      *================================================================*
      * COPYBOOK : MORISKP                                             *
      * PURPOSE  : LINKAGE FOR MONEY ORDERS RULE MODULE MORISK         *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1991-05-02 DPS  ORIGINAL                                       *
      * 1993-07-03 EWS  OUTSTAND RERUN PROTECTION CHG4325              *
      * 1994-12-08 MKL  ESCHEAT LIMIT REVISED INC8696                  *
      * 1994-08-12 LMB  AUDIT FINDING - FEE CONTROL TOTALS INC8555     *
      * 1997-02-09 CRP  OUTSTAND TABLE EXPANDED PRB6450                *
      * 1998-01-14 SKN  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1999-07-14 GAF  FACE LIMIT REVISED PRB4833                     *
      * 1999-11-23 MKL  FEE LIMIT REVISED INC9936                      *
      * 2000-05-06 TWH  ESCHEAT RERUN PROTECTION CHG5447               *
      * 2002-04-11 EWS  FACE TABLE EXPANDED CHG3090                    *
      * 2002-06-08 GAF  FEE TABLE EXPANDED REQ1326                     *
      * 2003-02-04 GAF  ESCHEAT CALCULATION CORRECTED REQ6792          *
      * 2005-05-13 PQR  OUTSTAND ROUNDING - HALF UP INC1455            *
      *================================================================*
       01  MORISK-PARMS.
           05  MORISK-ESCHEAT-YTD          PIC S9(13)V99 COMP-3.
           05  MORISK-OUTSTAND-YTD         PIC S9(13)V99 COMP-3.
           05  MORISK-PAID-AMT             PIC S9(13)V99 COMP-3.
           05  MORISK-FEE-AMT              PIC S9(13)V99 COMP-3.
           05  MORISK-FEE-YTD              PIC S9(13)V99 COMP-3.
           05  MORISK-OUTSTAND-AMT         PIC S9(13)V99 COMP-3.
           05  MORISK-NOTICE-DATE          PIC 9(08).
           05  MORISK-OPEN-DATE            PIC 9(08).
           05  MORISK-LAST-ACT-DATE        PIC 9(08).
           05  MORISK-EXPIRY-DATE          PIC 9(08).
           05  MORISK-REASON-CD            PIC X(01).
               88  MORISK-REASON-CD-B              VALUE 'B'.
               88  MORISK-REASON-CD-L              VALUE 'L'.
               88  MORISK-REASON-CD-P              VALUE 'P'.
               88  MORISK-REASON-CD-S              VALUE 'S'.
               88  MORISK-REASON-CD-T              VALUE 'T'.
               88  MORISK-REASON-CD-Z              VALUE 'Z'.
           05  MORISK-CYCLE-CD             PIC X(03).
               88  MORISK-CYCLE-CD-DGU             VALUE 'DGU'.
               88  MORISK-CYCLE-CD-GEJ             VALUE 'GEJ'.
               88  MORISK-CYCLE-CD-JBE             VALUE 'JBE'.
           05  MORISK-PAYEE-TYPE-CD        PIC X(02).
               88  MORISK-PAYEE-TYPE-CD-CJ         VALUE 'CJ'.
               88  MORISK-PAYEE-TYPE-CD-FY         VALUE 'FY'.
               88  MORISK-PAYEE-TYPE-CD-HX         VALUE 'HX'.
               88  MORISK-PAYEE-TYPE-CD-UV         VALUE 'UV'.
               88  MORISK-PAYEE-TYPE-CD-VV         VALUE 'VV'.
           05  MORISK-CHANNEL-CD           PIC X(02).
               88  MORISK-CHANNEL-CD-DR            VALUE 'DR'.
               88  MORISK-CHANNEL-CD-DX            VALUE 'DX'.
               88  MORISK-CHANNEL-CD-PT            VALUE 'PT'.
               88  MORISK-CHANNEL-CD-RN            VALUE 'RN'.
               88  MORISK-CHANNEL-CD-YD            VALUE 'YD'.
               88  MORISK-CHANNEL-CD-YU            VALUE 'YU'.
           05  MORISK-PREFERRED-FLAG       PIC X(01).
               88  MORISK-PREFERRED-FLAG-YES       VALUE 'Y'.
           05  MORISK-STOP-FLAG            PIC X(01).
               88  MORISK-STOP-FLAG-YES            VALUE 'Y'.
           05  MORISK-AUTO-RENEW-FLAG      PIC X(01).
               88  MORISK-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  MORISK-ITEM-CNT             PIC 9(07).
           05  MORISK-RETRY-CNT            PIC 9(07).
           05  MORISK-PENALTY-PCT          PIC 9(03)V9(06).
           05  MORISK-FEE-PCT              PIC 9(03)V9(06).
           05  MORISK-ADDR                 PIC X(30).
           05  MORISK-DESC                 PIC X(30).
           05  MORISK-RETURN-CODE          PIC 9(02).
