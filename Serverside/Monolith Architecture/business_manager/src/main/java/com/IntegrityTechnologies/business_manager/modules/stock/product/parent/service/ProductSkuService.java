package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSequence;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductSequenceRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
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

    private UUID branchId() {
        return BranchContext.get();
    }

    @Transactional
    public Long nextSequenceForCategory(Long categoryId) {

        Optional<ProductSequence> maybe =
                productSequenceRepository.findForUpdate(
                        categoryId,
                        tenantId(),
                        branchId()
                );

        ProductSequence seq;

        if (maybe.isPresent()) {

            seq = maybe.get();
            seq.setLastValue(seq.getLastValue() + 1);

        } else {

            seq = ProductSequence.builder()
                    .categoryId(categoryId)
                    .lastValue(1L)
                    .tenantId(tenantId())
                    .branchId(branchId())
                    .build();
        }

        productSequenceRepository.save(seq);

        return seq.getLastValue();
    }

    @Transactional
    public String generateSkuForCategory(Category category) {

        String raw = (category.getName() == null) ? "GEN" : category.getName();

        String alpha = raw.replaceAll("[^A-Za-z]", "");

        alpha = (alpha.length() >= 3)
                ? alpha.substring(0, 3).toUpperCase()
                : (alpha.toUpperCase() + "XXX").substring(0, 3);

        Long nextSeq = nextSequenceForCategory(category.getId());

        String seqStr = String.format("%06d", nextSeq);

        return alpha + "-" + seqStr;
    }
}