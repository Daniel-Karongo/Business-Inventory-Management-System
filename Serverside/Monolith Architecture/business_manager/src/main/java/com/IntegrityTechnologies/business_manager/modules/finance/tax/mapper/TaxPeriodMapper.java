package com.IntegrityTechnologies.business_manager.modules.finance.tax.mapper;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.dto.TaxPeriodDTO;

public final class TaxPeriodMapper {

    private TaxPeriodMapper() {
    }

    public static TaxPeriodDTO toDto(
            TaxPeriod period
    ) {
        return TaxPeriodDTO.builder()
                .id(period.getId())
                .startDate(period.getStartDate())
                .endDate(period.getEndDate())
                .closed(period.isClosed())
                .closedBy(period.getClosedBy())
                .build();
    }
}