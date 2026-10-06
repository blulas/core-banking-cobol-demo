      *================================================================*
      * COPYBOOK : ACOPNS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET ACOPNS MAP ACOPNM         *
      *            TRANSACTION CB03 - OPEN ACCOUNT                     *
      *            GENERATED FROM BMS SOURCE ACOPNS - DO NOT EDIT      *
      *================================================================*
       01  ACOPNMI.
           02  FILLER                      PIC X(12).
           02  TRANIDL                     PIC S9(4) COMP.
           02  TRANIDF                     PIC X.
           02  FILLER REDEFINES TRANIDF.
               03  TRANIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TRANIDI                     PIC X(004).
           02  TITLEL                      PIC S9(4) COMP.
           02  TITLEF                      PIC X.
           02  FILLER REDEFINES TITLEF.
               03  TITLEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TITLEI                      PIC X(040).
           02  CURDATEL                    PIC S9(4) COMP.
           02  CURDATEF                    PIC X.
           02  FILLER REDEFINES CURDATEF.
               03  CURDATEA                PIC X.
           02  FILLER                      PIC X(02).
           02  CURDATEI                    PIC X(010).
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  CNAMEL                      PIC S9(4) COMP.
           02  CNAMEF                      PIC X.
           02  FILLER REDEFINES CNAMEF.
               03  CNAMEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CNAMEI                      PIC X(040).
           02  PRODCDL                     PIC S9(4) COMP.
           02  PRODCDF                     PIC X.
           02  FILLER REDEFINES PRODCDF.
               03  PRODCDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PRODCDI                     PIC X(004).
           02  PRODNML                     PIC S9(4) COMP.
           02  PRODNMF                     PIC X.
           02  FILLER REDEFINES PRODNMF.
               03  PRODNMA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PRODNMI                     PIC X(030).
           02  JOINTIDL                    PIC S9(4) COMP.
           02  JOINTIDF                    PIC X.
           02  FILLER REDEFINES JOINTIDF.
               03  JOINTIDA                PIC X.
           02  FILLER                      PIC X(02).
           02  JOINTIDI                    PIC X(010).
           02  JNAMEL                      PIC S9(4) COMP.
           02  JNAMEF                      PIC X.
           02  FILLER REDEFINES JNAMEF.
               03  JNAMEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  JNAMEI                      PIC X(040).
           02  OWNTYPL                     PIC S9(4) COMP.
           02  OWNTYPF                     PIC X.
           02  FILLER REDEFINES OWNTYPF.
               03  OWNTYPA                 PIC X.
           02  FILLER                      PIC X(02).
           02  OWNTYPI                     PIC X(001).
           02  DEPAMTL                     PIC S9(4) COMP.
           02  DEPAMTF                     PIC X.
           02  FILLER REDEFINES DEPAMTF.
               03  DEPAMTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DEPAMTI                     PIC X(015).
           02  FUNDACTL                    PIC S9(4) COMP.
           02  FUNDACTF                    PIC X.
           02  FILLER REDEFINES FUNDACTF.
               03  FUNDACTA                PIC X.
           02  FILLER                      PIC X(02).
           02  FUNDACTI                    PIC X(012).
           02  ODOPTINL                    PIC S9(4) COMP.
           02  ODOPTINF                    PIC X.
           02  FILLER REDEFINES ODOPTINF.
               03  ODOPTINA                PIC X.
           02  FILLER                      PIC X(02).
           02  ODOPTINI                    PIC X(001).
           02  RENEWL                      PIC S9(4) COMP.
           02  RENEWF                      PIC X.
           02  FILLER REDEFINES RENEWF.
               03  RENEWA                  PIC X.
           02  FILLER                      PIC X(02).
           02  RENEWI                      PIC X(001).
           02  PAYACTL                     PIC S9(4) COMP.
           02  PAYACTF                     PIC X.
           02  FILLER REDEFINES PAYACTF.
               03  PAYACTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYACTI                     PIC X(012).
           02  ELIGDSCL                    PIC S9(4) COMP.
           02  ELIGDSCF                    PIC X.
           02  FILLER REDEFINES ELIGDSCF.
               03  ELIGDSCA                PIC X.
           02  FILLER                      PIC X(02).
           02  ELIGDSCI                    PIC X(060).
           02  DECLN1L                     PIC S9(4) COMP.
           02  DECLN1F                     PIC X.
           02  FILLER REDEFINES DECLN1F.
               03  DECLN1A                 PIC X.
           02  FILLER                      PIC X(02).
           02  DECLN1I                     PIC X(060).
           02  DECLN2L                     PIC S9(4) COMP.
           02  DECLN2F                     PIC X.
           02  FILLER REDEFINES DECLN2F.
               03  DECLN2A                 PIC X.
           02  FILLER                      PIC X(02).
           02  DECLN2I                     PIC X(060).
           02  RATEL                       PIC S9(4) COMP.
           02  RATEF                       PIC X.
           02  FILLER REDEFINES RATEF.
               03  RATEA                   PIC X.
           02  FILLER                      PIC X(02).
           02  RATEI                       PIC X(010).
           02  APYL                        PIC S9(4) COMP.
           02  APYF                        PIC X.
           02  FILLER REDEFINES APYF.
               03  APYA                    PIC X.
           02  FILLER                      PIC X(02).
           02  APYI                        PIC X(010).
           02  RTTIERL                     PIC S9(4) COMP.
           02  RTTIERF                     PIC X.
           02  FILLER REDEFINES RTTIERF.
               03  RTTIERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RTTIERI                     PIC X(004).
           02  ODLIML                      PIC S9(4) COMP.
           02  ODLIMF                      PIC X.
           02  FILLER REDEFINES ODLIMF.
               03  ODLIMA                  PIC X.
           02  FILLER                      PIC X(02).
           02  ODLIMI                      PIC X(012).
           02  MATDTL                      PIC S9(4) COMP.
           02  MATDTF                      PIC X.
           02  FILLER REDEFINES MATDTF.
               03  MATDTA                  PIC X.
           02  FILLER                      PIC X(02).
           02  MATDTI                      PIC X(010).
           02  NEWACCTL                    PIC S9(4) COMP.
           02  NEWACCTF                    PIC X.
           02  FILLER REDEFINES NEWACCTF.
               03  NEWACCTA                PIC X.
           02  FILLER                      PIC X(02).
           02  NEWACCTI                    PIC X(012).
           02  KTIERL                      PIC S9(4) COMP.
           02  KTIERF                      PIC X.
           02  FILLER REDEFINES KTIERF.
               03  KTIERA                  PIC X.
           02  FILLER                      PIC X(02).
           02  KTIERI                      PIC X(001).
           02  KREVDTL                     PIC S9(4) COMP.
           02  KREVDTF                     PIC X.
           02  FILLER REDEFINES KREVDTF.
               03  KREVDTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  KREVDTI                     PIC X(010).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
           02  PFKEYSL                     PIC S9(4) COMP.
           02  PFKEYSF                     PIC X.
           02  FILLER REDEFINES PFKEYSF.
               03  PFKEYSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PFKEYSI                     PIC X(079).
       01  ACOPNMO REDEFINES ACOPNMI.
           02  FILLER                      PIC X(12).
           02  FILLER                      PIC X(03).
           02  TRANIDC                     PIC X.
           02  TRANIDH                     PIC X.
           02  TRANIDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  TITLEC                      PIC X.
           02  TITLEH                      PIC X.
           02  TITLEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  CURDATEC                    PIC X.
           02  CURDATEH                    PIC X.
           02  CURDATEO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CNAMEC                      PIC X.
           02  CNAMEH                      PIC X.
           02  CNAMEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  PRODCDC                     PIC X.
           02  PRODCDH                     PIC X.
           02  PRODCDO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  PRODNMC                     PIC X.
           02  PRODNMH                     PIC X.
           02  PRODNMO                     PIC X(030).
           02  FILLER                      PIC X(03).
           02  JOINTIDC                    PIC X.
           02  JOINTIDH                    PIC X.
           02  JOINTIDO                    PIC X(010).
           02  FILLER                      PIC X(03).
           02  JNAMEC                      PIC X.
           02  JNAMEH                      PIC X.
           02  JNAMEO                      PIC X(040).
           02  FILLER                      PIC X(03).
           02  OWNTYPC                     PIC X.
           02  OWNTYPH                     PIC X.
           02  OWNTYPO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  DEPAMTC                     PIC X.
           02  DEPAMTH                     PIC X.
           02  DEPAMTO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  FUNDACTC                    PIC X.
           02  FUNDACTH                    PIC X.
           02  FUNDACTO                    PIC X(012).
           02  FILLER                      PIC X(03).
           02  ODOPTINC                    PIC X.
           02  ODOPTINH                    PIC X.
           02  ODOPTINO                    PIC X(001).
           02  FILLER                      PIC X(03).
           02  RENEWC                      PIC X.
           02  RENEWH                      PIC X.
           02  RENEWO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  PAYACTC                     PIC X.
           02  PAYACTH                     PIC X.
           02  PAYACTO                     PIC X(012).
           02  FILLER                      PIC X(03).
           02  ELIGDSCC                    PIC X.
           02  ELIGDSCH                    PIC X.
           02  ELIGDSCO                    PIC X(060).
           02  FILLER                      PIC X(03).
           02  DECLN1C                     PIC X.
           02  DECLN1H                     PIC X.
           02  DECLN1O                     PIC X(060).
           02  FILLER                      PIC X(03).
           02  DECLN2C                     PIC X.
           02  DECLN2H                     PIC X.
           02  DECLN2O                     PIC X(060).
           02  FILLER                      PIC X(03).
           02  RATEC                       PIC X.
           02  RATEH                       PIC X.
           02  RATEO                       PIC X(010).
           02  FILLER                      PIC X(03).
           02  APYC                        PIC X.
           02  APYH                        PIC X.
           02  APYO                        PIC X(010).
           02  FILLER                      PIC X(03).
           02  RTTIERC                     PIC X.
           02  RTTIERH                     PIC X.
           02  RTTIERO                     PIC X(004).
           02  FILLER                      PIC X(03).
           02  ODLIMC                      PIC X.
           02  ODLIMH                      PIC X.
           02  ODLIMO                      PIC X(012).
           02  FILLER                      PIC X(03).
           02  MATDTC                      PIC X.
           02  MATDTH                      PIC X.
           02  MATDTO                      PIC X(010).
           02  FILLER                      PIC X(03).
           02  NEWACCTC                    PIC X.
           02  NEWACCTH                    PIC X.
           02  NEWACCTO                    PIC X(012).
           02  FILLER                      PIC X(03).
           02  KTIERC                      PIC X.
           02  KTIERH                      PIC X.
           02  KTIERO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  KREVDTC                     PIC X.
           02  KREVDTH                     PIC X.
           02  KREVDTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
           02  FILLER                      PIC X(03).
           02  PFKEYSC                     PIC X.
           02  PFKEYSH                     PIC X.
           02  PFKEYSO                     PIC X(079).
