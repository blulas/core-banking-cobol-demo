      *================================================================*
      * COPYBOOK : PBSTATP                                             *
      * PURPOSE  : LINKAGE FOR PASSBOOK PRINTING RULE MODULE PBSTAT    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-10-17 BHS  ORIGINAL                                       *
      * 1998-01-26 DPS  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-01-06 GAF  LINE STATUS CODES ADDED INC1749                *
      * 1999-04-14 BHS  PRINTED RERUN PROTECTION CHG1464               *
      * 2000-08-13 SKN  AUDIT FINDING - CARRYFWD CONTROL TOTALS PRB0380*
      * 2002-02-28 DPS  AUDIT FINDING - CARRYFWD CONTROL TOTALS PRB2861*
      * 2004-06-06 EWS  PRINTED LIMIT REVISED REQ3381                  *
      * 2006-08-24 PQR  LINE RERUN PROTECTION PRB8417                  *
      * 2007-06-06 LMB  CARRYFWD EXCEPTIONS TO OPS QUEUE PRB8305       *
      * 2007-05-08 CRP  BALANCE LIMIT REVISED CHG3103                  *
      * 2008-05-02 EWS  BALANCE FEE SCHEDULE UPDATE PRB1599            *
      * 2012-08-07 GAF  BALANCE CUTOFF MOVED CHG1179                   *
      * 2012-08-03 DPS  UNPOSTED EXCEPTIONS TO OPS QUEUE PRB0763       *
      *================================================================*
       01  PBSTAT-PARMS.
           05  PBSTAT-UNPOSTED-YTD         PIC S9(13)V99 COMP-3.
           05  PBSTAT-BALANCE-AMT          PIC S9(13)V99 COMP-3.
           05  PBSTAT-LINE-YTD             PIC S9(13)V99 COMP-3.
           05  PBSTAT-CARRYFWD-YTD         PIC S9(13)V99 COMP-3.
           05  PBSTAT-UNPOSTED-AMT         PIC S9(13)V99 COMP-3.
           05  PBSTAT-LINE-AMT             PIC S9(13)V99 COMP-3.
           05  PBSTAT-CLOSE-DATE           PIC 9(08).
           05  PBSTAT-EFFECTIVE-DATE       PIC 9(08).
           05  PBSTAT-NEXT-DUE-DATE        PIC 9(08).
           05  PBSTAT-NEXT-REVIEW-DATE     PIC 9(08).
           05  PBSTAT-BOOK-TYPE-CD         PIC X(02).
               88  PBSTAT-BOOK-TYPE-CD-CH          VALUE 'CH'.
               88  PBSTAT-BOOK-TYPE-CD-HG          VALUE 'HG'.
               88  PBSTAT-BOOK-TYPE-CD-KB          VALUE 'KB'.
               88  PBSTAT-BOOK-TYPE-CD-KW          VALUE 'KW'.
               88  PBSTAT-BOOK-TYPE-CD-PA          VALUE 'PA'.
               88  PBSTAT-BOOK-TYPE-CD-TR          VALUE 'TR'.
           05  PBSTAT-PRINTER-CD           PIC X(02).
           05  PBSTAT-REGION-CD            PIC X(02).
               88  PBSTAT-REGION-CD-ER             VALUE 'ER'.
               88  PBSTAT-REGION-CD-GL             VALUE 'GL'.
               88  PBSTAT-REGION-CD-PE             VALUE 'PE'.
               88  PBSTAT-REGION-CD-RJ             VALUE 'RJ'.
               88  PBSTAT-REGION-CD-VB             VALUE 'VB'.
           05  PBSTAT-SOURCE-CD            PIC X(02).
               88  PBSTAT-SOURCE-CD-JX             VALUE 'JX'.
               88  PBSTAT-SOURCE-CD-ZC             VALUE 'ZC'.
               88  PBSTAT-SOURCE-CD-ZY             VALUE 'ZY'.
           05  PBSTAT-NOTICE-SENT-FLAG     PIC X(01).
               88  PBSTAT-NOTICE-SENT-FLAG-YES     VALUE 'Y'.
           05  PBSTAT-REVIEW-FLAG          PIC X(01).
               88  PBSTAT-REVIEW-FLAG-YES          VALUE 'Y'.
           05  PBSTAT-AUTO-RENEW-FLAG      PIC X(01).
               88  PBSTAT-AUTO-RENEW-FLAG-YES      VALUE 'Y'.
           05  PBSTAT-RETRY-CNT            PIC 9(07).
           05  PBSTAT-CYCLE-CNT            PIC 9(07).
           05  PBSTAT-PENALTY-PCT          PIC 9(03)V9(06).
           05  PBSTAT-MARGIN               PIC 9(03)V9(06).
           05  PBSTAT-CONTACT              PIC X(20).
           05  PBSTAT-CUST-ID              PIC X(10).
           05  PBSTAT-RETURN-CODE          PIC 9(02).
