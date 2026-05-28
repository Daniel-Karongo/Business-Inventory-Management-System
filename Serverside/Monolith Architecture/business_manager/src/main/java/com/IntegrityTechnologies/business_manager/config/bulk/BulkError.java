package com.IntegrityTechnologies.business_manager.config.bulk;

import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BulkError {

    private int row;

    private String field;

    private String code;

    private String message;

    private Object rejectedValue;

    @Builder.Default
    private BulkErrorSeverity severity =
            BulkErrorSeverity.ERROR;

    public BulkError(
            int row,
            String message
    ) {
        this.row = row;
        this.message = message;
        this.severity = BulkErrorSeverity.ERROR;
    }
}