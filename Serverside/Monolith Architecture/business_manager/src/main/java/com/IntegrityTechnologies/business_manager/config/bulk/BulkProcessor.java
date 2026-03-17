package com.IntegrityTechnologies.business_manager.config.bulk;

public interface BulkProcessor<I, O> {

    O process(
            I item,
            int row,
            BulkOptions options
    ) throws Exception;
}