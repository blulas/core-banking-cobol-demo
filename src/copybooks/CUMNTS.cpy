      *================================================================*
      * COPYBOOK : CUMNTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET CUMNTS MAP CUMNTM         *
      *            TRANSACTION CB02 - CUSTOMER MAINTENANCE             *
      *            GENERATED FROM BMS SOURCE CUMNTS - DO NOT EDIT      *
      *================================================================*
       01  CUMNTMI.
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
           02  CSTATL                      PIC S9(4) COMP.
           02  CSTATF                      PIC X.
           02  FILLER REDEFINES CSTATF.
               03  CSTATA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CSTATI                      PIC X(012).
           02  CMAINTL                     PIC S9(4) COMP.
           02  CMAINTF                     PIC X.
           02  FILLER REDEFINES CMAINTF.
               03  CMAINTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CMAINTI                     PIC X(015).
           02  LNAMEL                      PIC S9(4) COMP.
           02  LNAMEF                      PIC X.
           02  FILLER REDEFINES LNAMEF.
               03  LNAMEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  LNAMEI                      PIC X(030).
           02  FNAMEL                      PIC S9(4) COMP.
           02  FNAMEF                      PIC X.
           02  FILLER REDEFINES FNAMEF.
               03  FNAMEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  FNAMEI                      PIC X(020).
           02  MINITL                      PIC S9(4) COMP.
           02  MINITF                      PIC X.
           02  FILLER REDEFINES MINITF.
               03  MINITA                  PIC X.
           02  FILLER                      PIC X(02).
           02  MINITI                      PIC X(001).
           02  TAXIDL                      PIC S9(4) COMP.
           02  TAXIDF                      PIC X.
           02  FILLER REDEFINES TAXIDF.
               03  TAXIDA                  PIC X.
           02  FILLER                      PIC X(02).
           02  TAXIDI                      PIC X(009).
           02  TAXTYPL                     PIC S9(4) COMP.
           02  TAXTYPF                     PIC X.
           02  FILLER REDEFINES TAXTYPF.
               03  TAXTYPA                 PIC X.
           02  FILLER                      PIC X(02).
           02  TAXTYPI                     PIC X(001).
           02  PENDDSCL                    PIC S9(4) COMP.
           02  PENDDSCF                    PIC X.
           02  FILLER REDEFINES PENDDSCF.
               03  PENDDSCA                PIC X.
           02  FILLER                      PIC X(02).
           02  PENDDSCI                    PIC X(035).
           02  ADDR1L                      PIC S9(4) COMP.
           02  ADDR1F                      PIC X.
           02  FILLER REDEFINES ADDR1F.
               03  ADDR1A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ADDR1I                      PIC X(035).
           02  ADDR2L                      PIC S9(4) COMP.
           02  ADDR2F                      PIC X.
           02  FILLER REDEFINES ADDR2F.
               03  ADDR2A                  PIC X.
           02  FILLER                      PIC X(02).
           02  ADDR2I                      PIC X(035).
           02  CITYL                       PIC S9(4) COMP.
           02  CITYF                       PIC X.
           02  FILLER REDEFINES CITYF.
               03  CITYA                   PIC X.
           02  FILLER                      PIC X(02).
           02  CITYI                       PIC X(025).
           02  STATEL                      PIC S9(4) COMP.
           02  STATEF                      PIC X.
           02  FILLER REDEFINES STATEF.
               03  STATEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  STATEI                      PIC X(002).
           02  ZIPL                        PIC S9(4) COMP.
           02  ZIPF                        PIC X.
           02  FILLER REDEFINES ZIPF.
               03  ZIPA                    PIC X.
           02  FILLER                      PIC X(02).
           02  ZIPI                        PIC X(010).
           02  CTRYL                       PIC S9(4) COMP.
           02  CTRYF                       PIC X.
           02  FILLER REDEFINES CTRYF.
               03  CTRYA                   PIC X.
           02  FILLER                      PIC X(02).
           02  CTRYI                       PIC X(002).
           02  PHONEL                      PIC S9(4) COMP.
           02  PHONEF                      PIC X.
           02  FILLER REDEFINES PHONEF.
               03  PHONEA                  PIC X.
           02  FILLER                      PIC X(02).
           02  PHONEI                      PIC X(015).
           02  EMAILL                      PIC S9(4) COMP.
           02  EMAILF                      PIC X.
           02  FILLER REDEFINES EMAILF.
               03  EMAILA                  PIC X.
           02  FILLER                      PIC X(02).
           02  EMAILI                      PIC X(050).
           02  RISKOVRL                    PIC S9(4) COMP.
           02  RISKOVRF                    PIC X.
           02  FILLER REDEFINES RISKOVRF.
               03  RISKOVRA                PIC X.
           02  FILLER                      PIC X(02).
           02  RISKOVRI                    PIC X(001).
           02  CRISKL                      PIC S9(4) COMP.
           02  CRISKF                      PIC X.
           02  FILLER REDEFINES CRISKF.
               03  CRISKA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CRISKI                      PIC X(001).
           02  RSKRSNL                     PIC S9(4) COMP.
           02  RSKRSNF                     PIC X.
           02  FILLER REDEFINES RSKRSNF.
               03  RSKRSNA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RSKRSNI                     PIC X(020).
           02  CTIERL                      PIC S9(4) COMP.
           02  CTIERF                      PIC X.
           02  FILLER REDEFINES CTIERF.
               03  CTIERA                  PIC X.
           02  FILLER                      PIC X(02).
           02  CTIERI                      PIC X(001).
           02  CKYCL                       PIC S9(4) COMP.
           02  CKYCF                       PIC X.
           02  FILLER REDEFINES CKYCF.
               03  CKYCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  CKYCI                       PIC X(001).
           02  CKYCRVL                     PIC S9(4) COMP.
           02  CKYCRVF                     PIC X.
           02  FILLER REDEFINES CKYCRVF.
               03  CKYCRVA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CKYCRVI                     PIC X(010).
           02  RESULTL                     PIC S9(4) COMP.
           02  RESULTF                     PIC X.
           02  FILLER REDEFINES RESULTF.
               03  RESULTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RESULTI                     PIC X(069).
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
       01  CUMNTMO REDEFINES CUMNTMI.
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
           02  CSTATC                      PIC X.
           02  CSTATH                      PIC X.
           02  CSTATO                      PIC X(012).
           02  FILLER                      PIC X(03).
           02  CMAINTC                     PIC X.
           02  CMAINTH                     PIC X.
           02  CMAINTO                     PIC X(015).
           02  FILLER                      PIC X(03).
           02  LNAMEC                      PIC X.
           02  LNAMEH                      PIC X.
           02  LNAMEO                      PIC X(030).
           02  FILLER                      PIC X(03).
           02  FNAMEC                      PIC X.
           02  FNAMEH                      PIC X.
           02  FNAMEO                      PIC X(020).
           02  FILLER                      PIC X(03).
           02  MINITC                      PIC X.
           02  MINITH                      PIC X.
           02  MINITO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  TAXIDC                      PIC X.
           02  TAXIDH                      PIC X.
           02  TAXIDO                      PIC X(009).
           02  FILLER                      PIC X(03).
           02  TAXTYPC                     PIC X.
           02  TAXTYPH                     PIC X.
           02  TAXTYPO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  PENDDSCC                    PIC X.
           02  PENDDSCH                    PIC X.
           02  PENDDSCO                    PIC X(035).
           02  FILLER                      PIC X(03).
           02  ADDR1C                      PIC X.
           02  ADDR1H                      PIC X.
           02  ADDR1O                      PIC X(035).
           02  FILLER                      PIC X(03).
           02  ADDR2C                      PIC X.
           02  ADDR2H                      PIC X.
           02  ADDR2O                      PIC X(035).
           02  FILLER                      PIC X(03).
           02  CITYC                       PIC X.
           02  CITYH                       PIC X.
           02  CITYO                       PIC X(025).
           02  FILLER                      PIC X(03).
           02  STATEC                      PIC X.
           02  STATEH                      PIC X.
           02  STATEO                      PIC X(002).
           02  FILLER                      PIC X(03).
           02  ZIPC                        PIC X.
           02  ZIPH                        PIC X.
           02  ZIPO                        PIC X(010).
           02  FILLER                      PIC X(03).
           02  CTRYC                       PIC X.
           02  CTRYH                       PIC X.
           02  CTRYO                       PIC X(002).
           02  FILLER                      PIC X(03).
           02  PHONEC                      PIC X.
           02  PHONEH                      PIC X.
           02  PHONEO                      PIC X(015).
           02  FILLER                      PIC X(03).
           02  EMAILC                      PIC X.
           02  EMAILH                      PIC X.
           02  EMAILO                      PIC X(050).
           02  FILLER                      PIC X(03).
           02  RISKOVRC                    PIC X.
           02  RISKOVRH                    PIC X.
           02  RISKOVRO                    PIC X(001).
           02  FILLER                      PIC X(03).
           02  CRISKC                      PIC X.
           02  CRISKH                      PIC X.
           02  CRISKO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  RSKRSNC                     PIC X.
           02  RSKRSNH                     PIC X.
           02  RSKRSNO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  CTIERC                      PIC X.
           02  CTIERH                      PIC X.
           02  CTIERO                      PIC X(001).
           02  FILLER                      PIC X(03).
           02  CKYCC                       PIC X.
           02  CKYCH                       PIC X.
           02  CKYCO                       PIC X(001).
           02  FILLER                      PIC X(03).
           02  CKYCRVC                     PIC X.
           02  CKYCRVH                     PIC X.
           02  CKYCRVO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  RESULTC                     PIC X.
           02  RESULTH                     PIC X.
           02  RESULTO                     PIC X(069).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
           02  FILLER                      PIC X(03).
           02  PFKEYSC                     PIC X.
           02  PFKEYSH                     PIC X.
           02  PFKEYSO                     PIC X(079).
