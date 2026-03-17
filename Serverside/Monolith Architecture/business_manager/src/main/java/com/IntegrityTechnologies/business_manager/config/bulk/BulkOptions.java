package com.IntegrityTechnologies.business_manager.common.bulk;

import lombok.Data;

@Data
public class BulkOptions {
    private boolean dryRun = false;
    private boolean skipDuplicates = true;
    private boolean updateExisting;
    private boolean createMissingCategories = false;
    private boolean createMissingSuppliers = false;
}