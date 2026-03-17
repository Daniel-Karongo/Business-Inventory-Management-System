package com.IntegrityTechnologies.business_manager.common;

import java.util.List;

public record PageWrapper<T>(
        List<T> content,
        int pageNumber,
        int pageSize,
        long totalElements,
        int totalPages,
        boolean last
) {
    public PageWrapper(org.springframework.data.domain.Page<T> page) {
        this(page.getContent(), page.getNumber(), page.getSize(), page.getTotalElements(), page.getTotalPages(), page.isLast());
    }
}