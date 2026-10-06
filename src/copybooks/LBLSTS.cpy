      *================================================================*
      * COPYBOOK : LBLSTS                                              *
      * PURPOSE  : BMS SYMBOLIC MAP - MAPSET LBLSTS MAP LBLSTM         *
      *            TRANSACTION LB45 - LOCKBOX REMITTANCE               *
      *            GENERATED FROM BMS SOURCE LBLSTS - DO NOT EDIT      *
      *================================================================*
       01  LBLSTMI.
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
           02  REMITYL                     PIC S9(4) COMP.
           02  REMITYF                     PIC X.
           02  FILLER REDEFINES REMITYF.
               03  REMITYA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REMITYI                     PIC X(016).
           02  EXEMPTL                     PIC S9(4) COMP.
           02  EXEMPTF                     PIC X.
           02  FILLER REDEFINES EXEMPTF.
               03  EXEMPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXEMPTI                     PIC X(001).
           02  STOPFLL                     PIC S9(4) COMP.
           02  STOPFLF                     PIC X.
           02  FILLER REDEFINES STOPFLF.
               03  STOPFLA                 PIC X.
           02  FILLER                      PIC X(02).
           02  STOPFLI                     PIC X(001).
           02  DEPOSIL                     PIC S9(4) COMP.
           02  DEPOSIF                     PIC X.
           02  FILLER REDEFINES DEPOSIF.
               03  DEPOSIA                 PIC X.
           02  FILLER                      PIC X(02).
           02  DEPOSII                     PIC X(016).
           02  PAYMENL                     PIC S9(4) COMP.
           02  PAYMENF                     PIC X.
           02  FILLER REDEFINES PAYMENF.
               03  PAYMENA                 PIC X.
           02  FILLER                      PIC X(02).
           02  PAYMENI                     PIC X(007).
           02  AUTOREL                     PIC S9(4) COMP.
           02  AUTOREF                     PIC X.
           02  FILLER REDEFINES AUTOREF.
               03  AUTOREA                 PIC X.
           02  FILLER                      PIC X(02).
           02  AUTOREI                     PIC X(001).
           02  LOCKBOL                     PIC S9(4) COMP.
           02  LOCKBOF                     PIC X.
           02  FILLER REDEFINES LOCKBOF.
               03  LOCKBOA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LOCKBOI                     PIC X(002).
           02  LASTSTL                     PIC S9(4) COMP.
           02  LASTSTF                     PIC X.
           02  FILLER REDEFINES LASTSTF.
               03  LASTSTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  LASTSTI                     PIC X(010).
           02  DESCL                       PIC S9(4) COMP.
           02  DESCF                       PIC X.
           02  FILLER REDEFINES DESCF.
               03  DESCA                   PIC X.
           02  FILLER                      PIC X(02).
           02  DESCI                       PIC X(030).
           02  REMITAL                     PIC S9(4) COMP.
           02  REMITAF                     PIC X.
           02  FILLER REDEFINES REMITAF.
               03  REMITAA                 PIC X.
           02  FILLER                      PIC X(02).
           02  REMITAI                     PIC X(016).
           02  EXCEPTL                     PIC S9(4) COMP.
           02  EXCEPTF                     PIC X.
           02  FILLER REDEFINES EXCEPTF.
               03  EXCEPTA                 PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPTI                     PIC X(016).
           02  EXCEPT1L                    PIC S9(4) COMP.
           02  EXCEPT1F                    PIC X.
           02  FILLER REDEFINES EXCEPT1F.
               03  EXCEPT1A                PIC X.
           02  FILLER                      PIC X(02).
           02  EXCEPT1I                    PIC X(007).
           02  REFNOL                      PIC S9(4) COMP.
           02  REFNOF                      PIC X.
           02  FILLER REDEFINES REFNOF.
               03  REFNOA                  PIC X.
           02  FILLER                      PIC X(02).
           02  REFNOI                      PIC X(016).
           02  MSGL                        PIC S9(4) COMP.
           02  MSGF                        PIC X.
           02  FILLER REDEFINES MSGF.
               03  MSGA                    PIC X.
           02  FILLER                      PIC X(02).
           02  MSGI                        PIC X(079).
       01  LBLSTMO REDEFINES LBLSTMI.
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
           02  REMITYC                     PIC X.
           02  REMITYH                     PIC X.
           02  REMITYO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXEMPTC                     PIC X.
           02  EXEMPTH                     PIC X.
           02  EXEMPTO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  STOPFLC                     PIC X.
           02  STOPFLH                     PIC X.
           02  STOPFLO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  DEPOSIC                     PIC X.
           02  DEPOSIH                     PIC X.
           02  DEPOSIO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  PAYMENC                     PIC X.
           02  PAYMENH                     PIC X.
           02  PAYMENO                     PIC X(007).
           02  FILLER                      PIC X(03).
           02  AUTOREC                     PIC X.
           02  AUTOREH                     PIC X.
           02  AUTOREO                     PIC X(001).
           02  FILLER                      PIC X(03).
           02  LOCKBOC                     PIC X.
           02  LOCKBOH                     PIC X.
           02  LOCKBOO                     PIC X(002).
           02  FILLER                      PIC X(03).
           02  LASTSTC                     PIC X.
           02  LASTSTH                     PIC X.
           02  LASTSTO                     PIC X(010).
           02  FILLER                      PIC X(03).
           02  DESCC                       PIC X.
           02  DESCH                       PIC X.
           02  DESCO                       PIC X(030).
           02  FILLER                      PIC X(03).
           02  REMITAC                     PIC X.
           02  REMITAH                     PIC X.
           02  REMITAO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXCEPTC                     PIC X.
           02  EXCEPTH                     PIC X.
           02  EXCEPTO                     PIC X(016).
           02  FILLER                      PIC X(03).
           02  EXCEPT1C                    PIC X.
           02  EXCEPT1H                    PIC X.
           02  EXCEPT1O                    PIC X(007).
           02  FILLER                      PIC X(03).
           02  REFNOC                      PIC X.
           02  REFNOH                      PIC X.
           02  REFNOO                      PIC X(016).
           02  FILLER                      PIC X(03).
           02  MSGC                        PIC X.
           02  MSGH                        PIC X.
           02  MSGO                        PIC X(079).
