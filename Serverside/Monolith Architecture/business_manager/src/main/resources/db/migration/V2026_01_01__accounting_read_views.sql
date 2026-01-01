CREATE OR REPLACE VIEW vw_trial_balance AS
SELECT
    a.id            AS account_id,
    a.code          AS account_code,
    a.name          AS account_name,
    a.type          AS account_type,

    SUM(CASE WHEN le.direction = 'DEBIT'  THEN le.amount ELSE 0 END) AS total_debit,
    SUM(CASE WHEN le.direction = 'CREDIT' THEN le.amount ELSE 0 END) AS total_credit

FROM ledger_entries le
         JOIN journal_entries je ON je.id = le.journal_entry_id
         JOIN accounts a         ON a.id  = le.account_id

WHERE je.posted_at IS NOT NULL
GROUP BY a.id, a.code, a.name, a.type;




CREATE OR REPLACE VIEW vw_general_ledger AS
SELECT
    le.id                   AS ledger_entry_id,
    a.id                    AS account_id,
    a.code                  AS account_code,
    a.name                  AS account_name,

    je.posted_at             AS posting_date,
    je.reference             AS journal_reference,
    je.description           AS journal_description,

    CASE WHEN le.direction = 'DEBIT'  THEN le.amount ELSE 0 END AS debit,
    CASE WHEN le.direction = 'CREDIT' THEN le.amount ELSE 0 END AS credit

FROM ledger_entries le
         JOIN journal_entries je ON je.id = le.journal_entry_id
         JOIN accounts a         ON a.id  = le.account_id

WHERE je.posted_at IS NOT NULL;


CREATE OR REPLACE VIEW vw_profit_and_loss AS
SELECT
    a.type AS account_type,

    SUM(
            CASE
                WHEN a.type = 'INCOME'  AND le.direction = 'CREDIT' THEN le.amount
                WHEN a.type = 'EXPENSE' AND le.direction = 'DEBIT'  THEN le.amount
                ELSE 0
                END
        ) AS amount

FROM ledger_entries le
         JOIN journal_entries je ON je.id = le.journal_entry_id
         JOIN accounts a         ON a.id  = le.account_id

WHERE
    je.posted_at IS NOT NULL
  AND a.type IN ('INCOME', 'EXPENSE')

GROUP BY a.type;