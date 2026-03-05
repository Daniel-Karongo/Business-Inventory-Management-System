CREATE TABLE IF NOT EXISTS daily_account_balance_snapshot (

    id BINARY(16) PRIMARY KEY,

    branch_id BINARY(16) NOT NULL,
    account_id BINARY(16) NOT NULL,

    snapshot_date DATE NOT NULL,

    balance DECIMAL(19,2) NOT NULL,

    created_at DATETIME NOT NULL,

    UNIQUE KEY uk_snapshot (branch_id, account_id, snapshot_date),

    INDEX idx_snapshot_branch_date (branch_id, snapshot_date),
    INDEX idx_snapshot_account (account_id)
    );