package com.IntegrityTechnologies.business_manager.config.bulk;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkRequest<T> {
    private List<T> items = new ArrayList<>();
    private BulkOptions options;
}