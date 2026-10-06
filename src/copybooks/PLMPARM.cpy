      *================================================================*
      * COPYBOOK : PLMPARM                                             *
      * PURPOSE  : LINKAGE FOR PAYLIMIT - OUTBOUND PAYMENT LIMITS AND  *
      *            APPROVAL REQUIREMENTS                               *
      * INPUT    : PAYMENT TYPE / AMOUNT, CUSTOMER SEGMENT, ACCOUNT    *
      *            OPEN DATE, AMOUNTS ALREADY RELEASED TODAY FOR THIS  *
      *            ACCOUNT (EXCLUDING THIS PAYMENT), ACCOUNT LIMITS    *
      *            (USED FOR CB/IN SEGMENTS) AND BENEFICIARY COUNTRY   *
      * OUTPUT   : DECISION A APPROVE / N NEEDS APPROVAL / D DECLINE   *
      * RETURN   : 00 APPROVE  04 NEEDS APPROVAL  08 DECLINE  12 ERROR *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-02-12 RJT  ORIGINAL                                       *
      * 2006-10-02 BHS  DUAL APPROVAL FLAG (SOX 404)                   *
      * 2012-05-14 DPS  NEW ACCOUNT ACH LIMIT (FRAUD INC11873)         *
      *================================================================*
       01  PAY-LIMIT-PARMS.
           05  PLM-PAY-TYPE                PIC X(01).
               88  PLM-BOOK                          VALUE 'B'.
               88  PLM-ACH                           VALUE 'A'.
               88  PLM-SDACH                         VALUE 'S'.
               88  PLM-WIRE                          VALUE 'W'.
               88  PLM-INTL-WIRE                     VALUE 'I'.
               88  PLM-ANY-ACH                       VALUE 'A' 'S'.
               88  PLM-ANY-WIRE                      VALUE 'W' 'I'.
           05  PLM-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  PLM-SEGMENT                 PIC X(02).
           05  PLM-ACCT-OPEN-DATE          PIC 9(08).
           05  PLM-BUSINESS-DATE           PIC 9(08).
           05  PLM-ACH-USED-TODAY          PIC S9(13)V99 COMP-3.
           05  PLM-WIRE-USED-TODAY         PIC S9(13)V99 COMP-3.
           05  PLM-BOOK-USED-TODAY         PIC S9(13)V99 COMP-3.
           05  PLM-ACCT-ACH-LIMIT          PIC S9(13)V99 COMP-3.
           05  PLM-ACCT-WIRE-LIMIT         PIC S9(13)V99 COMP-3.
           05  PLM-BENE-COUNTRY            PIC X(02).
      *--- OUTPUT -----------------------------------------------------*
           05  PLM-DECISION                PIC X(01).
               88  PLM-APPROVE                       VALUE 'A'.
               88  PLM-NEEDS-APPROVAL                VALUE 'N'.
               88  PLM-DECLINE                       VALUE 'D'.
      *        OKAY LTXN LDAY LNEW LOVR NEWW DUAL HRCY AMTZ TYPE
           05  PLM-REASON-CODE             PIC X(04).
           05  PLM-REASON-TEXT             PIC X(50).
           05  PLM-DUAL-APPROVAL-FLAG      PIC X(01).
               88  PLM-DUAL-REQUIRED                 VALUE 'Y'.
           05  PLM-COMPLIANCE-FLAG         PIC X(01).
               88  PLM-COMPLIANCE-REQUIRED           VALUE 'Y'.
           05  PLM-NEW-ACCT-FLAG           PIC X(01).
           05  PLM-TXN-LIMIT               PIC S9(13)V99 COMP-3.
           05  PLM-DAILY-LIMIT             PIC S9(13)V99 COMP-3.
           05  PLM-DAILY-REMAINING         PIC S9(13)V99 COMP-3.
           05  PLM-DUAL-THRESHOLD          PIC S9(13)V99 COMP-3.
           05  PLM-RETURN-CODE             PIC 9(02).
