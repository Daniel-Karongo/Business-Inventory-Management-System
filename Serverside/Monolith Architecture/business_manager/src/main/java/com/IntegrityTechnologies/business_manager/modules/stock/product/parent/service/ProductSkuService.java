package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSequence;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductSequenceRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ProductSkuService {

    private final ProductSequenceRepository productSequenceRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public Long nextSequenceForCategory(
            Long categoryId,
            UUID branchId
    ) {

        ProductSequence seq =
                productSequenceRepository.findForUpdate(
                        categoryId,
                        tenantId(),
                        branchId
                ).orElse(null);

        if (seq == null) {

            try {

                seq = productSequenceRepository.saveAndFlush(
                        ProductSequence.builder()
                                .categoryId(categoryId)
                                .lastValue(0L)
                                .tenantId(tenantId())
                                .branchId(branchId)
                                .build()
                );

            } catch (DataIntegrityViolationException ex) {

                seq =
                        productSequenceRepository.findForUpdate(
                                categoryId,
                                tenantId(),
                                branchId
                        ).orElseThrow();
            }
        }

        seq.setLastValue(seq.getLastValue() + 1);

        productSequenceRepository.saveAndFlush(seq);

        return seq.getLastValue();
    }

    @Transactional
    public String generateSkuForCategory(
            Category category,
            UUID branchId
    ) {

        String raw =
                (category.getName() == null)
                        ? "GEN"
                        : category.getName();

        String alpha =
                raw.replaceAll("[^A-Za-z]", "");

        alpha =
                (alpha.length() >= 3)
                        ? alpha.substring(0, 3).toUpperCase()
                        : (alpha.toUpperCase() + "XXX")
                        .substring(0, 3);

        Long nextSeq =
                nextSequenceForCategory(
                        category.getId(),
                        branchId
                );

        String seqStr =
                String.format("%06d", nextSeq);

        return alpha + "-" + seqStr;
    }
}