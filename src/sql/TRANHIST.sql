-- =====================================================================
-- TABLE    : CBS.TRAN_HISTORY
-- PURPOSE  : Online-queryable copy of posted monetary transactions.
--            Loaded nightly from the TRANHIST sequential file (EODCTL).
--            Used by CICS inquiry transactions (ACCTINQ, CUSTINQ...).
-- CHANGE LOG
--   2004-06-14 BHS  Original
--   2016-09-23 DPS  Added CHANNEL for same-day ACH
-- =====================================================================
CREATE TABLE CBS.TRAN_HISTORY
  ( TRAN_ID              CHAR(16)       NOT NULL
  , ACCT_NUMBER          CHAR(12)       NOT NULL
  , EFFECTIVE_DATE       DATE           NOT NULL
  , POST_DATE            DATE           NOT NULL
  , TRAN_CODE            CHAR(4)        NOT NULL
  , DR_CR                CHAR(1)        NOT NULL
  , AMOUNT               DECIMAL(15,2)  NOT NULL
  , RUNNING_BALANCE      DECIMAL(15,2)
  , CHANNEL              CHAR(3)        NOT NULL WITH DEFAULT 'BAT'
  , DESCRIPTION          VARCHAR(40)
  , REFERENCE            CHAR(20)
  , COUNTERPARTY_NAME    VARCHAR(35)
  , STATUS               CHAR(1)        NOT NULL WITH DEFAULT 'S'
  , PRIMARY KEY (TRAN_ID)
  ) IN CBSDB01.TSTRNHST;

CREATE INDEX CBS.XTRNHST1
    ON CBS.TRAN_HISTORY (ACCT_NUMBER, POST_DATE DESC, TRAN_ID DESC);
