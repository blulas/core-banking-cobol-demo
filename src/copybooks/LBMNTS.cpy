      *================================================================*
      * COPYBOOK : LBMNTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET LBMNTS MAP LBMNTM         *
      *            TRANSACTION LB75 - LOCKBOX REMITTANCE               *
      *            GENERATED FROM BMS SOURCE LBMNTS - DO NOT EDIT      *
      *================================================================*
       01  LBMNTMI.
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
           02  ADDRL                       PIC S9(4) COMP.
           02  ADDRF                       PIC X.
           02  FILLER REDEFINES ADDRF.
               03  ADDRA                   PIC X.
           02  FILLER                      PIC X(02).
           02  ADDRI                       PIC X(030).
           02  DEPOSIL                     PIC S9(4) COMP.
           02  DEPOSIF                     PIC X.
           02  FILLER REDEFINES DEPOSIF.
               03  DEPOSIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DEPOSII                     PIC X(016).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(001).
           02  BATCHTL                     PIC S9(4) COMP.
           02  BATCHTF                     PIC X.
           02  FILLER REDEFINES BATCHTF.
               03  BATCHTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BATCHTI                     PIC X(001).
           02  PREFERL                     PIC S9(4) COMP.
           02  PREFERF                     PIC X.
           02  FILLER REDEFINES PREFERF.
               03  PREFERA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PREFERI                     PIC X(001).
           02  CUSTIDL                     PIC S9(4) COMP.
           02  CUSTIDF                     PIC X.
           02  FILLER REDEFINES CUSTIDF.
               03  CUSTIDA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CUSTIDI                     PIC X(010).
           02  MATURIL                     PIC S9(4) COMP.
           02  MATURIF                     PIC X.
           02  FILLER REDEFINES MATURIF.
               03  MATURIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  MATURII                     PIC X(010).
           02  HOLDFLL                     PIC S9(4) COMP.
           02  HOLDFLF                     PIC X.
           02  FILLER REDEFINES HOLDFLF.
               03  HOLDFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  HOLDFLI                     PIC X(001).
           02  NEXTDUL                     PIC S9(4) COMP.
           02  NEXTDUF                     PIC X.
           02  FILLER REDEFINES NEXTDUF.
               03  NEXTDUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NEXTDUI                     PIC X(010).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(016).
           02  LASTPAL                     PIC S9(4) COMP.
           02  LASTPAF                     PIC X.
           02  FILLER REDEFINES LASTPAF.
               03  LASTPAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTPAI                     PIC X(010).
           02  CONTACL                     PIC S9(4) COMP.
           02  CONTACF                     PIC X.
           02  FILLER REDEFINES CONTACF.
               03  CONTACA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CONTACI                     PIC X(020).
           02  CASHONL                     PIC S9(4) COMP.
           02  CASHONF                     PIC X.
           02  FILLER REDEFINES CASHONF.
               03  CASHONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CASHONI                     PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  LBMNTMO REDEFINES LBMNTMI.
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
           02  ADDRC                       PIC X.
           02  ADDRH                       PIC X.
           02  ADDRO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  DEPOSIC                     PIC X.
           02  DEPOSIH                     PIC X.
           02  DEPOSIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  BATCHTC                     PIC X.
           02  BATCHTH                     PIC X.
           02  BATCHTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  PREFERC                     PIC X.
           02  PREFERH                     PIC X.
           02  PREFERO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CUSTIDC                     PIC X.
           02  CUSTIDH                     PIC X.
           02  CUSTIDO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  MATURIC                     PIC X.
           02  MATURIH                     PIC X.
           02  MATURIO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  HOLDFLC                     PIC X.
           02  HOLDFLH                     PIC X.
           02  HOLDFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  NEXTDUC                     PIC X.
           02  NEXTDUH                     PIC X.
           02  NEXTDUO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  LASTPAC                     PIC X.
           02  LASTPAH                     PIC X.
           02  LASTPAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  CONTACC                     PIC X.
           02  CONTACH                     PIC X.
           02  CONTACO                     PIC X(020).
           02  FILLER                      PIC X(03).
           02  CASHONC                     PIC X.
           02  CASHONH                     PIC X.
           02  CASHONO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
