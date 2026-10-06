      *================================================================*
      * COPYBOOK : TLMNTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET TLMNTS MAP TLMNTM         *
      *            TRANSACTION TL31 - TELLER BALANCING                 *
      *            GENERATED FROM BMS SOURCE TLMNTS - DO NOT EDIT      *
      *================================================================*
       01  TLMNTMI.
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
           02  STOPFLL                     PIC S9(4) COMP.
           02  STOPFLF                     PIC X.
           02  FILLER REDEFINES STOPFLF.
               03  STOPFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STOPFLI                     PIC X(001).
           02  CASHOUL                     PIC S9(4) COMP.
           02  CASHOUF                     PIC X.
           02  FILLER REDEFINES CASHOUF.
               03  CASHOUA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CASHOUI                     PIC X(016).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(007).
           02  CASHINL                     PIC S9(4) COMP.
           02  CASHINF                     PIC X.
           02  FILLER REDEFINES CASHINF.
               03  CASHINA                 PIC X.
           02  FILLER                      PIC X(02).
           02  CASHINI                     PIC X(016).
           02  DESCL                       PIC S9(4) COMP.
           02  DESCF                       PIC X.
           02  FILLER REDEFINES DESCF.
               03  DESCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  DESCI                       PIC X(030).
           02  NOTICEL                     PIC S9(4) COMP.
           02  NOTICEF                     PIC X.
           02  FILLER REDEFINES NOTICEF.
               03  NOTICEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  NOTICEI                     PIC X(010).
           02  REASONL                     PIC S9(4) COMP.
           02  REASONF                     PIC X.
           02  FILLER REDEFINES REASONF.
               03  REASONA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REASONI                     PIC X(002).
           02  SUSPENL                     PIC S9(4) COMP.
           02  SUSPENF                     PIC X.
           02  FILLER REDEFINES SUSPENF.
               03  SUSPENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SUSPENI                     PIC X(001).
           02  RETRYCL                     PIC S9(4) COMP.
           02  RETRYCF                     PIC X.
           02  FILLER REDEFINES RETRYCF.
               03  RETRYCA                 PIC X.
           02  FILLER                      PIC X(02).
           02  RETRYCI                     PIC X(007).
           02  SOURCEL                     PIC S9(4) COMP.
           02  SOURCEF                     PIC X.
           02  FILLER REDEFINES SOURCEF.
               03  SOURCEA                 PIC X.
           02  FILLER                      PIC X(02).
           02  SOURCEI                     PIC X(002).
           02  PROOFTL                     PIC S9(4) COMP.
           02  PROOFTF                     PIC X.
           02  FILLER REDEFINES PROOFTF.
               03  PROOFTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PROOFTI                     PIC X(002).
           02  BATCHAL                     PIC S9(4) COMP.
           02  BATCHAF                     PIC X.
           02  FILLER REDEFINES BATCHAF.
               03  BATCHAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  BATCHAI                     PIC X(016).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  TLMNTMO REDEFINES TLMNTMI.
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
           02  STOPFLC                     PIC X.
           02  STOPFLH                     PIC X.
           02  STOPFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  CASHOUC                     PIC X.
           02  CASHOUH                     PIC X.
           02  CASHOUO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  CASHINC                     PIC X.
           02  CASHINH                     PIC X.
           02  CASHINO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  DESCC                       PIC X.
           02  DESCH                       PIC X.
           02  DESCO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  NOTICEC                     PIC X.
           02  NOTICEH                     PIC X.
           02  NOTICEO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  REASONC                     PIC X.
           02  REASONH                     PIC X.
           02  REASONO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  SUSPENC                     PIC X.
           02  SUSPENH                     PIC X.
           02  SUSPENO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  RETRYCC                     PIC X.
           02  RETRYCH                     PIC X.
           02  RETRYCO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  SOURCEC                     PIC X.
           02  SOURCEH                     PIC X.
           02  SOURCEO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  PROOFTC                     PIC X.
           02  PROOFTH                     PIC X.
           02  PROOFTO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  BATCHAC                     PIC X.
           02  BATCHAH                     PIC X.
           02  BATCHAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
