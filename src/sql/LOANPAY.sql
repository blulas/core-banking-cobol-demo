-- =====================================================================
-- TABLE    : CBS.LOAN_PAYMENT_HIST
-- PURPOSE  : Online-queryable history of loan payments and how each
--            payment was applied (fees / interest / principal /
--            suspense). One row per applied payment.
--            Written by LNPAYAP to the LNPAYHST sequential file
--            (copybook LNPHREC) and loaded nightly by the DSNUTILB
--            LOAD RESUME YES step in job CBSLOAN.
--            Read by CICS transaction CB40 (LNINQ) - most recent first.
-- CHANGE LOG
--   2008-10-14 DPS  Original - online payment history for CB40
--   2012-04-30 DPS  Added APPLIED_SUSPENSE (non-accrual cash basis)
--                   CHG1187
--   2016-04-25 AKW  Added CHANNEL for LPER autopay / online payments
-- =====================================================================
CREATE TABLE CBS.LOAN_PAYMENT_HIST
  ( LOAN_NUMBER          CHAR(12)       NOT NULL
  , PAYMENT_DATE         DATE           NOT NULL
  , TRAN_ID              CHAR(16)       NOT NULL
  , SOURCE_ACCT          CHAR(12)       NOT NULL WITH DEFAULT
  , PAYMENT_AMT          DECIMAL(15,2)  NOT NULL
  , APPLIED_FEES         DECIMAL(9,2)   NOT NULL WITH DEFAULT 0
  , APPLIED_INT          DECIMAL(13,2)  NOT NULL WITH DEFAULT 0
  , APPLIED_PRIN         DECIMAL(15,2)  NOT NULL WITH DEFAULT 0
  , APPLIED_SUSPENSE     DECIMAL(13,2)  NOT NULL WITH DEFAULT 0
  , PRIN_BAL_AFTER       DECIMAL(15,2)  NOT NULL
  , WATERFALL            CHAR(1)        NOT NULL WITH DEFAULT 'S'
  , PAYOFF_FLAG          CHAR(1)        NOT NULL WITH DEFAULT 'N'
  , CHANNEL              CHAR(3)        NOT NULL WITH DEFAULT 'BAT'
  , PRIMARY KEY (LOAN_NUMBER, PAYMENT_DATE, TRAN_ID)
  ) IN CBSDB01.TSLNPHST;

CREATE UNIQUE INDEX CBS.XLNPHST0
    ON CBS.LOAN_PAYMENT_HIST (LOAN_NUMBER, PAYMENT_DATE, TRAN_ID)
    CLUSTER;

CREATE INDEX CBS.XLNPHST1
    ON CBS.LOAN_PAYMENT_HIST (LOAN_NUMBER, PAYMENT_DATE DESC);
