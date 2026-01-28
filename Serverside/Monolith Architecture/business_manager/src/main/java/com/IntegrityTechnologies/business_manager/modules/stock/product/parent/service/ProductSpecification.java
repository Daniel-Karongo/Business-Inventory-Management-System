package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.criteria.Predicate;
import org.springframework.data.jpa.domain.Specification;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class ProductSpecification {

    public static Specification<Product> filterProducts(
            List<Long> categoryIds,
            String name,
            String description,
            String keyword,
            BigDecimal minPrice,
            BigDecimal maxPrice
    ) {
        return (root, query, cb) -> {

            List<Predicate> predicates = new ArrayList<>();

            // JOIN category ONCE (safe even if unused)
            var categoryJoin = root.join("category", jakarta.persistence.criteria.JoinType.LEFT);

            if (categoryIds != null && !categoryIds.isEmpty()) {
                predicates.add(categoryJoin.get("id").in(categoryIds));
            }

            if (name != null && !name.isBlank()) {
                predicates.add(cb.like(
                        cb.lower(root.get("name")),
                        "%" + name.toLowerCase() + "%"
                ));
            }

            if (description != null && !description.isBlank()) {
                predicates.add(cb.like(
                        cb.lower(root.get("description")),
                        "%" + description.toLowerCase() + "%"
                ));
            }

            // 🔥 KEYWORD SEARCH (name OR sku OR category)
            if (keyword != null && !keyword.isBlank()) {
                String kw = "%" + keyword.toLowerCase() + "%";

                predicates.add(cb.or(
                        cb.like(cb.lower(root.get("name")), kw),
                        cb.like(cb.lower(root.get("sku")), kw),
                        cb.like(cb.lower(categoryJoin.get("name")), kw)
                ));
            }

            if (minPrice != null) {
                predicates.add(cb.greaterThanOrEqualTo(root.get("price"), minPrice));
            }

            if (maxPrice != null) {
                predicates.add(cb.lessThanOrEqualTo(root.get("price"), maxPrice));
            }

            return cb.and(predicates.toArray(new Predicate[0]));
        };
    }
}