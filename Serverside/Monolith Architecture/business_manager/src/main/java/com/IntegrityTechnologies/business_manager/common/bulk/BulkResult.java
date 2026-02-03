package com.IntegrityTechnologies.business_manager.common.bulk;

import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BulkResult<T> {

    private int total;
    private int success;
    private int failed;

    private List<T> data = new ArrayList<>();
    private List<BulkError> errors = new ArrayList<>();

    public void addError(int row, String message) {
        errors.add(new BulkError(row, message));
        failed++;
    }

    public void addSuccess(T item) {
        data.add(item);
        success++;
    }
}