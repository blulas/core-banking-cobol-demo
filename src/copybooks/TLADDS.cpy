      *================================================================*
      * COPYBOOK : TLADDS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET TLADDS MAP TLADDM         *
      *            TRANSACTION TL23 - TELLER BALANCING                 *
      *            GENERATED FROM BMS SOURCE TLADDS - DO NOT EDIT      *
      *================================================================*
       01  TLADDMI.
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
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  VAULTSL                     PIC S9(4) COMP.
           02  VAULTSF                     PIC X.
           02  FILLER REDEFINES VAULTSF.
               03  VAULTSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  VAULTSI                     PIC X(016).
           02  SOURCEL                     PIC S9(4) COMP.
           02  SOURCEF                     PIC X.
           02  FILLER REDEFINES SOURCEF.
               03  SOURCEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SOURCEI                     PIC X(002).
           02  RETRYCL                     PIC S9(4) COMP.
           02  RETRYCF                     PIC X.
           02  FILLER REDEFINES RETRYCF.
               03  RETRYCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RETRYCI                     PIC X(007).
           02  REVIEWL                     PIC S9(4) COMP.
           02  REVIEWF                     PIC X.
           02  FILLER REDEFINES REVIEWF.
               03  REVIEWA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REVIEWI                     PIC X(001).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(002).
           02  EXPIRYL                     PIC S9(4) COMP.
           02  EXPIRYF                     PIC X.
           02  FILLER REDEFINES EXPIRYF.
               03  EXPIRYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXPIRYI                     PIC X(010).
           02  CASHINL                     PIC S9(4) COMP.
           02  CASHINF                     PIC X.
           02  FILLER REDEFINES CASHINF.
               03  CASHINA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CASHINI                     PIC X(016).
           02  LASTSTL                     PIC S9(4) COMP.
           02  LASTSTF                     PIC X.
           02  FILLER REDEFINES LASTSTF.
               03  LASTSTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTSTI                     PIC X(010).
           02  HOLDFLL                     PIC S9(4) COMP.
           02  HOLDFLF                     PIC X.
           02  FILLER REDEFINES HOLDFLF.
               03  HOLDFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  HOLDFLI                     PIC X(001).
           02  VAULTS1L                    PIC S9(4) COMP.
           02  VAULTS1F                    PIC X.
           02  FILLER REDEFINES VAULTS1F.
               03  VAULTS1A                PIC X.
           02  FILLER                      PIC X(02).
           02  VAULTS1I                    PIC X(016).
           02  VAULTBL                     PIC S9(4) COMP.
           02  VAULTBF                     PIC X.
           02  FILLER REDEFINES VAULTBF.
               03  VAULTBA                 PIC X.
           02  FILLER                      PIC X(02).
           02  VAULTBI                     PIC X(016).
           02  CHANNEL                     PIC S9(4) COMP.
           02  CHANNEF                     PIC X.
           02  FILLER REDEFINES CHANNEF.
               03  CHANNEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHANNEI                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  TLADDMO REDEFINES TLADDMI.
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
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  VAULTSC                     PIC X.
           02  VAULTSH                     PIC X.
           02  VAULTSO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  SOURCEC                     PIC X.
           02  SOURCEH                     PIC X.
           02  SOURCEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  RETRYCC                     PIC X.
           02  RETRYCH                     PIC X.
           02  RETRYCO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  REVIEWC                     PIC X.
           02  REVIEWH                     PIC X.
           02  REVIEWO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  EXPIRYC                     PIC X.
           02  EXPIRYH                     PIC X.
           02  EXPIRYO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CASHINC                     PIC X.
           02  CASHINH                     PIC X.
           02  CASHINO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  LASTSTC                     PIC X.
           02  LASTSTH                     PIC X.
           02  LASTSTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  HOLDFLC                     PIC X.
           02  HOLDFLH                     PIC X.
           02  HOLDFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  VAULTS1C                    PIC X.
           02  VAULTS1H                    PIC X.
           02  VAULTS1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  VAULTBC                     PIC X.
           02  VAULTBH                     PIC X.
           02  VAULTBO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CHANNEC                     PIC X.
           02  CHANNEH                     PIC X.
           02  CHANNEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
