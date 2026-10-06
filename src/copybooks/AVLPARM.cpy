      *================================================================*
      * COPYBOOK : AVLPARM                                             *
      * PURPOSE  : LINKAGE FOR ACHVALID - NACHA ENTRY VALIDATION       *
      * INPUT    : ROUTING (8 DIGIT DFI ID + CHECK DIGIT), TRANSACTION *
      *            CODE, AMOUNT, SEC CODE, RECEIVING ACCOUNT TYPE      *
      *            (I CONSUMER / C CORPORATE, SPACE = CONSUMER) AND    *
      *            AUTHORIZATION-ON-FILE FLAG (WEB DEBITS)             *
      * OUTPUT   : RESULT A ACCEPT / W ACCEPT WITH WARNING / R REJECT, *
      *            INTERNAL REASON, NACHA RETURN REASON (RNN), DR/CR,  *
      *            ACCOUNT KIND (D DDA / S SAVINGS), PRENOTE FLAG      *
      * RETURN   : 00 ACCEPT  04 WARNING  08 REJECT                    *
      *----------------------------------------------------------------*
      * CHANGE LOG                                                     *
      * 1996-02-12 RJT  ORIGINAL                                       *
      * 2013-09-16 DPS  WEB AUTHORIZATION, CTX                         *
      *================================================================*
       01  ACH-VALID-PARMS.
           05  AVL-ROUTING                 PIC X(09).
           05  AVL-TRAN-CODE               PIC X(02).
           05  AVL-AMOUNT                  PIC S9(13)V99 COMP-3.
           05  AVL-SEC-CODE                PIC X(03).
           05  AVL-ACCT-TYPE               PIC X(01).
               88  AVL-CONSUMER-ACCT                 VALUE 'I' ' '.
               88  AVL-CORPORATE-ACCT                VALUE 'C'.
           05  AVL-AUTH-FLAG               PIC X(01).
               88  AVL-AUTH-ON-FILE                  VALUE 'Y'.
      *--- OUTPUT -----------------------------------------------------*
           05  AVL-RESULT                  PIC X(01).
               88  AVL-ACCEPT                        VALUE 'A'.
               88  AVL-WARNING                       VALUE 'W'.
               88  AVL-REJECT                        VALUE 'R'.
      *        IATX SECU RTNN RTPX RTCK TCOD PREA AMT0 AMTX
      *        SECX SECW WEBC WEBA OKAY
           05  AVL-REASON-CODE             PIC X(04).
           05  AVL-RETURN-REASON           PIC X(03).
           05  AVL-REASON-TEXT             PIC X(40).
           05  AVL-DR-CR                   PIC X(01).
           05  AVL-ACCT-KIND               PIC X(01).
           05  AVL-PRENOTE-FLAG            PIC X(01).
               88  AVL-IS-PRENOTE                    VALUE 'Y'.
           05  AVL-COMPUTED-CHECK-DIGIT    PIC 9(01).
           05  AVL-RETURN-CODE             PIC 9(02).
