      *================================================================*
      * COPYBOOK : PPMNTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET PPMNTS MAP PPMNTM         *
      *            TRANSACTION PP90 - POSITIVE PAY                     *
      *            GENERATED FROM BMS SOURCE PPMNTS - DO NOT EDIT      *
      *================================================================*
       01  PPMNTMI.
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
           02  PAYMENL                     PIC S9(4) COMP.
           02  PAYMENF                     PIC X.
           02  FILLER REDEFINES PAYMENF.
               03  PAYMENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYMENI                     PIC X(007).
           02  STALEYL                     PIC S9(4) COMP.
           02  STALEYF                     PIC X.
           02  FILLER REDEFINES STALEYF.
               03  STALEYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STALEYI                     PIC X(016).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(016).
           02  STALEAL                     PIC S9(4) COMP.
           02  STALEAF                     PIC X.
           02  FILLER REDEFINES STALEAF.
               03  STALEAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STALEAI                     PIC X(016).
           02  EXCEPT1L                    PIC S9(4) COMP.
           02  EXCEPT1F                    PIC X.
           02  FILLER REDEFINES EXCEPT1F.
               03  EXCEPT1A                PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPT1I                    PIC X(007).
           02  PAIDYTL                     PIC S9(4) COMP.
           02  PAIDYTF                     PIC X.
           02  FILLER REDEFINES PAIDYTF.
               03  PAIDYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAIDYTI                     PIC X(016).
           02  STATUSL                     PIC S9(4) COMP.
           02  STATUSF                     PIC X.
           02  FILLER REDEFINES STATUSF.
               03  STATUSA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STATUSI                     PIC X(002).
           02  MATCHRL                     PIC S9(4) COMP.
           02  MATCHRF                     PIC X.
           02  FILLER REDEFINES MATCHRF.
               03  MATCHRA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MATCHRI                     PIC X(001).
           02  LASTMAL                     PIC S9(4) COMP.
           02  LASTMAF                     PIC X.
           02  FILLER REDEFINES LASTMAF.
               03  LASTMAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTMAI                     PIC X(010).
           02  ISSUEDL                     PIC S9(4) COMP.
           02  ISSUEDF                     PIC X.
           02  FILLER REDEFINES ISSUEDF.
               03  ISSUEDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  ISSUEDI                     PIC X(016).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(001).
           02  REFNOL                      PIC S9(4) COMP.
           02  REFNOF                      PIC X.
           02  FILLER REDEFINES REFNOF.
               03  REFNOA                  PIC X.
           02  FILLER                      PIC X(02).
           02  REFNOI                      PIC X(016).
           02  ISSUED1L                    PIC S9(4) COMP.
           02  ISSUED1F                    PIC X.
           02  FILLER REDEFINES ISSUED1F.
               03  ISSUED1A                PIC X.
           02  FILLER                      PIC X(02).
           02  ISSUED1I                    PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  PPMNTMO REDEFINES PPMNTMI.
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
           02  PAYMENC                     PIC X.
           02  PAYMENH                     PIC X.
           02  PAYMENO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  STALEYC                     PIC X.
           02  STALEYH                     PIC X.
           02  STALEYO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  STALEAC                     PIC X.
           02  STALEAH                     PIC X.
           02  STALEAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXCEPT1C                    PIC X.
           02  EXCEPT1H                    PIC X.
           02  EXCEPT1O                    PIC X(007).
           02  FILLER                      PIC X(03).
           02  PAIDYTC                     PIC X.
           02  PAIDYTH                     PIC X.
           02  PAIDYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  STATUSC                     PIC X.
           02  STATUSH                     PIC X.
           02  STATUSO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  MATCHRC                     PIC X.
           02  MATCHRH                     PIC X.
           02  MATCHRO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  LASTMAC                     PIC X.
           02  LASTMAH                     PIC X.
           02  LASTMAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  ISSUEDC                     PIC X.
           02  ISSUEDH                     PIC X.
           02  ISSUEDO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  REFNOC                      PIC X.
           02  REFNOH                      PIC X.
           02  REFNOO                      PIC X(016).
           02  FILLER                      PIC X(03).
           02  ISSUED1C                    PIC X.
           02  ISSUED1H                    PIC X.
           02  ISSUED1O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
