      *================================================================*
      * COPYBOOK : PBMNTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET PBMNTS MAP PBMNTM         *
      *            TRANSACTION PB66 - PASSBOOK PRINTING                *
      *            GENERATED FROM BMS SOURCE PBMNTS - DO NOT EDIT      *
      *================================================================*
       01  PBMNTMI.
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
           02  UNPOSTL                     PIC S9(4) COMP.
           02  UNPOSTF                     PIC X.
           02  FILLER REDEFINES UNPOSTF.
               03  UNPOSTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  UNPOSTI                     PIC X(016).
           02  CYCLECL                     PIC S9(4) COMP.
           02  CYCLECF                     PIC X.
           02  FILLER REDEFINES CYCLECF.
               03  CYCLECA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CYCLECI                     PIC X(007).
           02  INTEREL                     PIC S9(4) COMP.
           02  INTEREF                     PIC X.
           02  FILLER REDEFINES INTEREF.
               03  INTEREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  INTEREI                     PIC X(016).
           02  LINEYTL                     PIC S9(4) COMP.
           02  LINEYTF                     PIC X.
           02  FILLER REDEFINES LINEYTF.
               03  LINEYTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LINEYTI                     PIC X(016).
           02  PRINTEL                     PIC S9(4) COMP.
           02  PRINTEF                     PIC X.
           02  FILLER REDEFINES PRINTEF.
               03  PRINTEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PRINTEI                     PIC X(016).
           02  PAYMENL                     PIC S9(4) COMP.
           02  PAYMENF                     PIC X.
           02  FILLER REDEFINES PAYMENF.
               03  PAYMENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYMENI                     PIC X(007).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(001).
           02  PRINTE1L                    PIC S9(4) COMP.
           02  PRINTE1F                    PIC X.
           02  FILLER REDEFINES PRINTE1F.
               03  PRINTE1A                PIC X.
           02  FILLER                      PIC X(02).
           02  PRINTE1I                    PIC X(002).
           02  RETRYCL                     PIC S9(4) COMP.
           02  RETRYCF                     PIC X.
           02  FILLER REDEFINES RETRYCF.
               03  RETRYCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RETRYCI                     PIC X(007).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  PRINTE2L                    PIC S9(4) COMP.
           02  PRINTE2F                    PIC X.
           02  FILLER REDEFINES PRINTE2F.
               03  PRINTE2A                PIC X.
           02  FILLER                      PIC X(02).
           02  PRINTE2I                    PIC X(016).
           02  CHANNEL                     PIC S9(4) COMP.
           02  CHANNEF                     PIC X.
           02  FILLER REDEFINES CHANNEF.
               03  CHANNEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CHANNEI                     PIC X(001).
           02  LASTMAL                     PIC S9(4) COMP.
           02  LASTMAF                     PIC X.
           02  FILLER REDEFINES LASTMAF.
               03  LASTMAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTMAI                     PIC X(010).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  PBMNTMO REDEFINES PBMNTMI.
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
           02  UNPOSTC                     PIC X.
           02  UNPOSTH                     PIC X.
           02  UNPOSTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  CYCLECC                     PIC X.
           02  CYCLECH                     PIC X.
           02  CYCLECO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  INTEREC                     PIC X.
           02  INTEREH                     PIC X.
           02  INTEREO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  LINEYTC                     PIC X.
           02  LINEYTH                     PIC X.
           02  LINEYTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  PRINTEC                     PIC X.
           02  PRINTEH                     PIC X.
           02  PRINTEO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  PAYMENC                     PIC X.
           02  PAYMENH                     PIC X.
           02  PAYMENO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  PRINTE1C                    PIC X.
           02  PRINTE1H                    PIC X.
           02  PRINTE1O                    PIC X(002).
           02  FILLER                      PIC X(03).
           02  RETRYCC                     PIC X.
           02  RETRYCH                     PIC X.
           02  RETRYCO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  PRINTE2C                    PIC X.
           02  PRINTE2H                    PIC X.
           02  PRINTE2O                    PIC X(016).
           02  FILLER                      PIC X(03).
           02  CHANNEC                     PIC X.
           02  CHANNEH                     PIC X.
           02  CHANNEO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  LASTMAC                     PIC X.
           02  LASTMAH                     PIC X.
           02  LASTMAO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
