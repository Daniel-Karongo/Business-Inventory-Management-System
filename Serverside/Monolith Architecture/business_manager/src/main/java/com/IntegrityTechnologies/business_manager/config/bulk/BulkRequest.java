package com.IntegrityTechnologies.business_manager.config.bulk;

import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BulkRequest<T> {
    private List<T> items = new ArrayList<>();
    private BulkOptions options;
}