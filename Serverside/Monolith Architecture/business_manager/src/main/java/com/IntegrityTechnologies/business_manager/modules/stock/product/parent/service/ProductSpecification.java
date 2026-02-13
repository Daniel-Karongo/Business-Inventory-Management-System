package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
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

            query.distinct(true); // ✅ Prevent duplicates when joins expand

            List<Predicate> predicates = new ArrayList<>();

            Join<Object, Object> categoryJoin = null;

            boolean needsCategoryJoin =
                    (categoryIds != null && !categoryIds.isEmpty()) ||
                            (keyword != null && !keyword.isBlank());

            if (needsCategoryJoin) {
                categoryJoin = root.join("category", JoinType.LEFT);
            }

            // ✅ Category filter
            if (categoryIds != null && !categoryIds.isEmpty()) {
                predicates.add(categoryJoin.get("id").in(categoryIds));
            }

            // ✅ Name filter
            if (name != null && !name.isBlank()) {
                predicates.add(cb.like(
                        cb.lower(root.get("name")),
                        "%" + name.toLowerCase() + "%"
                ));
            }

            // ✅ Description filter
            if (description != null && !description.isBlank()) {
                predicates.add(cb.like(
                        cb.lower(root.get("description")),
                        "%" + description.toLowerCase() + "%"
                ));
            }

            // ✅ Keyword (name OR sku OR category)
            if (keyword != null && !keyword.isBlank()) {
                String kw = "%" + keyword.toLowerCase() + "%";

                predicates.add(cb.or(
                        cb.like(cb.lower(root.get("name")), kw),
                        cb.like(cb.lower(root.get("sku")), kw),
                        cb.like(cb.lower(categoryJoin.get("name")), kw)
                ));
            }

            // ✅ Price filters
            if (minPrice != null) {
                predicates.add(cb.greaterThanOrEqualTo(
                        root.get("price"),
                        minPrice
                ));
            }

            if (maxPrice != null) {
                predicates.add(cb.lessThanOrEqualTo(
                        root.get("price"),
                        maxPrice
                ));
            }

            return cb.and(predicates.toArray(new Predicate[0]));
        };
    }
}