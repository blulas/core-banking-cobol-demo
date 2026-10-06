      *================================================================*
      * COPYBOOK : TCPRCP                                              *
      * PURPOSE  : LINKAGE FOR TRAVELERS CHEQUES RULE MODULE TCPRC     *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1986-11-24 BHS  ORIGINAL                                       *
      * 1993-11-12 SKN  CONSIGN EXCEPTIONS TO OPS QUEUE CHG0404        *
      * 1995-12-05 BHS  COMMISSION LIMIT REVISED PRB2495               *
      * 1998-03-23 TWH  Y2K - DATES EXPANDED TO CCYYMMDD               *
      * 1998-03-26 CRP  SHORTAGE STATUS CODES ADDED PRB2915            *
      * 2004-04-12 CRP  REMIT LIMIT REVISED INC3244                    *
      * 2009-04-05 RJT  FACE NOTICE TEXT PER LEGAL PRB5945             *
      * 2010-12-03 PQR  REFUND EXCEPTIONS TO OPS QUEUE INC6408         *
      * 2013-04-10 SKN  CONSIGN LIMIT REVISED CHG9504                  *
      *================================================================*
       01  TCPRC-PARMS.
           05  TCPRC-REMIT-YTD             PIC S9(13)V99 COMP-3.
           05  TCPRC-REFUND-YTD            PIC S9(13)V99 COMP-3.
           05  TCPRC-CONSIGN-YTD           PIC S9(13)V99 COMP-3.
           05  TCPRC-SHORTAGE-YTD          PIC S9(13)V99 COMP-3.
           05  TCPRC-FACE-YTD              PIC S9(13)V99 COMP-3.
           05  TCPRC-CONSIGN-AMT           PIC S9(13)V99 COMP-3.
           05  TCPRC-MATURITY-DATE         PIC 9(08).
           05  TCPRC-CLOSE-DATE            PIC 9(08).
           05  TCPRC-NEXT-REVIEW-DATE      PIC 9(08).
           05  TCPRC-EXPIRY-DATE           PIC 9(08).
           05  TCPRC-STATUS-CD             PIC X(02).
               88  TCPRC-STATUS-CD-HK              VALUE 'HK'.
               88  TCPRC-STATUS-CD-RU              VALUE 'RU'.
               88  TCPRC-STATUS-CD-WA              VALUE 'WA'.
           05  TCPRC-BRANCH-CD             PIC X(02).
               88  TCPRC-BRANCH-CD-EG              VALUE 'EG'.
               88  TCPRC-BRANCH-CD-HZ              VALUE 'HZ'.
               88  TCPRC-BRANCH-CD-JF              VALUE 'JF'.
               88  TCPRC-BRANCH-CD-NZ              VALUE 'NZ'.
           05  TCPRC-CYCLE-CD              PIC X(03).
           05  TCPRC-ISSUER-CD             PIC X(02).
               88  TCPRC-ISSUER-CD-AF              VALUE 'AF'.
               88  TCPRC-ISSUER-CD-CJ              VALUE 'CJ'.
               88  TCPRC-ISSUER-CD-DU              VALUE 'DU'.
               88  TCPRC-ISSUER-CD-JY              VALUE 'JY'.
               88  TCPRC-ISSUER-CD-RR              VALUE 'RR'.
               88  TCPRC-ISSUER-CD-TK              VALUE 'TK'.
           05  TCPRC-REVIEW-FLAG           PIC X(01).
               88  TCPRC-REVIEW-FLAG-YES           VALUE 'Y'.
           05  TCPRC-NOTICE-SENT-FLAG      PIC X(01).
               88  TCPRC-NOTICE-SENT-FLAG-YES      VALUE 'Y'.
           05  TCPRC-SUSPEND-FLAG          PIC X(01).
               88  TCPRC-SUSPEND-FLAG-YES          VALUE 'Y'.
           05  TCPRC-EXCEPT-CNT            PIC 9(07).
           05  TCPRC-CYCLE-CNT             PIC 9(07).
           05  TCPRC-FEE-PCT               PIC 9(03)V9(06).
           05  TCPRC-MARGIN                PIC 9(03)V9(06).
           05  TCPRC-NAME                  PIC X(30).
           05  TCPRC-CONTACT               PIC X(20).
           05  TCPRC-RETURN-CODE           PIC 9(02).
