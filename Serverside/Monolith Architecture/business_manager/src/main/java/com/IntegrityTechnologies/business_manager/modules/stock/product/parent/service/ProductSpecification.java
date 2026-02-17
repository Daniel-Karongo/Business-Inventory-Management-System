package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import jakarta.persistence.criteria.*;
import org.springframework.data.jpa.domain.Specification;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class ProductSpecification {

    public static Specification<Product> filterProducts(
            List<Long> categoryIds,
            String name,
            String description,
            String keyword,
            BigDecimal minPrice,
            BigDecimal maxPrice,
            Boolean deleted,
            boolean includeDeleted,
            String sortBy,
            String direction,
            Integer minSuppliers,
            Integer maxSuppliers,
            UUID supplierId
    ) {

        return (root, query, cb) -> {

            List<Predicate> predicates = new ArrayList<>();

            Join<Product, Category> categoryJoin =
                    root.join("category", JoinType.LEFT);

            /* =============================
               BASIC FILTERS
               ============================= */

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

            if (keyword != null && !keyword.isBlank()) {
                String kw = "%" + keyword.toLowerCase() + "%";
                predicates.add(cb.or(
                        cb.like(cb.lower(root.get("name")), kw),
                        cb.like(cb.lower(root.get("sku")), kw),
                        cb.like(cb.lower(categoryJoin.get("name")), kw)
                ));
            }

            if (deleted != null) {
                predicates.add(cb.equal(root.get("deleted"), deleted));
            }

            if (!includeDeleted && deleted == null) {
                predicates.add(cb.isFalse(root.get("deleted")));
            }

            if (supplierId != null) {
                Join<Product, Supplier> supplierJoin =
                        root.join("suppliers", JoinType.LEFT);

                predicates.add(
                        cb.equal(supplierJoin.get("id"), supplierId)
                );
            }

            /* =============================
               🔥 SUPPLIER COUNT FILTER
               ============================= */

            if (minSuppliers != null || maxSuppliers != null) {

                Subquery<Long> supplierCountSub = query.subquery(Long.class);
                Root<Product> subRoot = supplierCountSub.correlate(root);
                Join<Product, Supplier> supplierJoin =
                        subRoot.join("suppliers", JoinType.LEFT);

                supplierCountSub.select(cb.count(supplierJoin.get("id")));

                if (minSuppliers != null) {
                    predicates.add(
                            cb.greaterThanOrEqualTo(
                                    supplierCountSub,
                                    minSuppliers.longValue()
                            )
                    );
                }

                if (maxSuppliers != null) {
                    predicates.add(
                            cb.lessThanOrEqualTo(
                                    supplierCountSub,
                                    maxSuppliers.longValue()
                            )
                    );
                }
            }

            /* =============================
               SORTING
               ============================= */

            if (sortBy != null) {

                boolean desc = "desc".equalsIgnoreCase(direction);

                // Variant Count
                if ("variantCount".equals(sortBy)) {

                    Subquery<Long> variantSub = query.subquery(Long.class);
                    Root<ProductVariant> v = variantSub.from(ProductVariant.class);

                    variantSub.select(cb.count(v.get("id")))
                            .where(cb.equal(v.get("product"), root));

                    query.orderBy(desc
                            ? cb.desc(variantSub)
                            : cb.asc(variantSub)
                    );
                }

                // Supplier Count Sorting
                else if ("supplierCount".equals(sortBy)) {

                    Subquery<Long> supplierSub = query.subquery(Long.class);
                    Root<Product> subRoot = supplierSub.correlate(root);
                    Join<Product, Supplier> s =
                            subRoot.join("suppliers", JoinType.LEFT);

                    supplierSub.select(cb.count(s.get("id")));

                    query.orderBy(desc
                            ? cb.desc(supplierSub)
                            : cb.asc(supplierSub)
                    );
                }

                // Category Name
                else if ("category.name".equals(sortBy)) {

                    query.orderBy(desc
                            ? cb.desc(categoryJoin.get("name"))
                            : cb.asc(categoryJoin.get("name"))
                    );
                }

                // Hybrid Date
                else if ("updatedAt".equals(sortBy)) {

                    Expression<?> hybrid =
                            cb.coalesce(
                                    root.get("updatedAt"),
                                    root.get("createdAt")
                            );

                    query.orderBy(desc
                            ? cb.desc(hybrid)
                            : cb.asc(hybrid)
                    );
                }

                else {
                    query.orderBy(desc
                            ? cb.desc(root.get(sortBy))
                            : cb.asc(root.get(sortBy))
                    );
                }
            }

            return cb.and(predicates.toArray(new Predicate[0]));
        };
    }
}