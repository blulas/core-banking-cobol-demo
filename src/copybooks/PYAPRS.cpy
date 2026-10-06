      *================================================================*
      * COPYBOOK : PYAPRS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET PYAPRS MAP PYAPRM         *
      *            TRANSACTION PY13 - PAYROLL SERVICES                 *
      *            GENERATED FROM BMS SOURCE PYAPRS - DO NOT EDIT      *
      *================================================================*
       01  PYAPRMI.
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
           02  KEYL                        PIC S9(4) COMP.
           02  KEYF                        PIC X.
           02  FILLER REDEFINES KEYF.
               03  KEYA                    PIC X.
           02  FILLER                      PIC X(02).
           02  KEYI                        PIC X(012).
           02  NEXTDUL                     PIC S9(4) COMP.
           02  NEXTDUF                     PIC X.
           02  FILLER REDEFINES NEXTDUF.
               03  NEXTDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTDUI                     PIC X(010).
           02  CLOSEDL                     PIC S9(4) COMP.
           02  CLOSEDF                     PIC X.
           02  FILLER REDEFINES CLOSEDF.
               03  CLOSEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CLOSEDI                     PIC X(010).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  FICAYTL                     PIC S9(4) COMP.
           02  FICAYTF                     PIC X.
           02  FILLER REDEFINES FICAYTF.
               03  FICAYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FICAYTI                     PIC X(016).
           02  FEDTAXL                     PIC S9(4) COMP.
           02  FEDTAXF                     PIC X.
           02  FILLER REDEFINES FEDTAXF.
               03  FEDTAXA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEDTAXI                     PIC X(016).
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  STATETL                     PIC S9(4) COMP.
           02  STATETF                     PIC X.
           02  FILLER REDEFINES STATETF.
               03  STATETA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STATETI                     PIC X(016).
           02  FEEPCTL                     PIC S9(4) COMP.
           02  FEEPCTF                     PIC X.
           02  FILLER REDEFINES FEEPCTF.
               03  FEEPCTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  FEEPCTI                     PIC X(010).
           02  EXPIRYL                     PIC S9(4) COMP.
           02  EXPIRYF                     PIC X.
           02  FILLER REDEFINES EXPIRYF.
               03  EXPIRYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXPIRYI                     PIC X(010).
           02  DEDUCTL                     PIC S9(4) COMP.
           02  DEDUCTF                     PIC X.
           02  FILLER REDEFINES DEDUCTF.
               03  DEDUCTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DEDUCTI                     PIC X(016).
           02  SOURCEL                     PIC S9(4) COMP.
           02  SOURCEF                     PIC X.
           02  FILLER REDEFINES SOURCEF.
               03  SOURCEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SOURCEI                     PIC X(002).
           02  DESCL                       PIC S9(4) COMP.
           02  DESCF                       PIC X.
           02  FILLER REDEFINES DESCF.
               03  DESCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  DESCI                       PIC X(030).
           02  GROSSAL                     PIC S9(4) COMP.
           02  GROSSAF                     PIC X.
           02  FILLER REDEFINES GROSSAF.
               03  GROSSAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  GROSSAI                     PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  PYAPRMO REDEFINES PYAPRMI.
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
           02  KEYC                        PIC X.
           02  KEYH                        PIC X.
           02  KEYO                        PIC X(012).
           02  FILLER                      PIC X(03).
           02  NEXTDUC                     PIC X.
           02  NEXTDUH                     PIC X.
           02  NEXTDUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CLOSEDC                     PIC X.
           02  CLOSEDH                     PIC X.
           02  CLOSEDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  FICAYTC                     PIC X.
           02  FICAYTH                     PIC X.
           02  FICAYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  FEDTAXC                     PIC X.
           02  FEDTAXH                     PIC X.
           02  FEDTAXO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  STATETC                     PIC X.
           02  STATETH                     PIC X.
           02  STATETO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  FEEPCTC                     PIC X.
           02  FEEPCTH                     PIC X.
           02  FEEPCTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  EXPIRYC                     PIC X.
           02  EXPIRYH                     PIC X.
           02  EXPIRYO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  DEDUCTC                     PIC X.
           02  DEDUCTH                     PIC X.
           02  DEDUCTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  SOURCEC                     PIC X.
           02  SOURCEH                     PIC X.
           02  SOURCEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  DESCC                       PIC X.
           02  DESCH                       PIC X.
           02  DESCO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  GROSSAC                     PIC X.
           02  GROSSAH                     PIC X.
           02  GROSSAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
