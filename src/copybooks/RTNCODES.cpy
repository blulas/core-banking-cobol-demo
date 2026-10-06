      *================================================================*
      * COPYBOOK : RTNCODES                                            *
      * PURPOSE  : STANDARD RETURN CODES FOR CALLED RULE MODULES AND   *
      *            STANDARD FILE STATUS CONDITION NAMES                *
      *================================================================*
       01  WS-STANDARD-RETURN-CODES.
           05  RC-OK                       PIC 9(02) VALUE 00.
           05  RC-WARNING                  PIC 9(02) VALUE 04.
           05  RC-DECLINED                 PIC 9(02) VALUE 08.
           05  RC-ERROR                    PIC 9(02) VALUE 12.
           05  RC-SEVERE                   PIC 9(02) VALUE 16.
