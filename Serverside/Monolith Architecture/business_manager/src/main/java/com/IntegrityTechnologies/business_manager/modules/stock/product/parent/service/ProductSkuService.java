package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.ProductSequence;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductSequenceRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
public class ProductSkuService {

    @Autowired
    private ProductSequenceRepository productSequenceRepository;

    @Transactional
    public Long nextSequenceForCategory(Long categoryId) {
        Optional<ProductSequence> maybe = productSequenceRepository.findByCategoryIdForUpdate(categoryId);
        ProductSequence seq;

        if (maybe.isPresent()) {
            seq = maybe.get();
            seq.setLastValue(seq.getLastValue() + 1);
        } else {
            seq = new ProductSequence();
            seq.setCategoryId(categoryId);
            seq.setLastValue(1L);
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