ALTER TABLE ledger_entries
    PARTITION BY HASH(branch_id)
    PARTITIONS 64;

ALTER TABLE journal_entries
    PARTITION BY HASH(branch_id)
    PARTITIONS 64;