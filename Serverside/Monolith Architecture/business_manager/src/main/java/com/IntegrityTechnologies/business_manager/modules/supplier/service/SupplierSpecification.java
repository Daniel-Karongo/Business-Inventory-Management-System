package com.IntegrityTechnologies.business_manager.modules.supplier.service;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;

public class SupplierSpecification {

    public static Specification<Supplier> hasNameLike(String name) {
        return (root, query, cb) ->
                name == null ? null : cb.like(cb.lower(root.get("name")), "%" + name.toLowerCase() + "%");
    }

    public static Specification<Supplier> hasEmail(String email) {
        return (root, query, cb) ->
                email == null ? null : cb.equal(cb.lower(root.get("email")), email.toLowerCase());
    }

    public static Specification<Supplier> hasPhone(String phone) {
        return (root, query, cb) ->
                phone == null ? null : cb.equal(root.get("phoneNumber"), phone);
    }

    public static Specification<Supplier> inCategories(List<Long> categoryIds) {
        return (root, query, cb) -> {
            if (categoryIds == null || categoryIds.isEmpty()) return null;
            query.distinct(true);
            return root.join("categories").get("id").in(categoryIds);
        };
    }

    public static Specification<Supplier> hasMinRating(Double minRating) {
        return (root, query, cb) ->
                minRating == null ? null : cb.greaterThanOrEqualTo(root.get("rating"), minRating);
    }

    public static Specification<Supplier> fromRegion(String region) {
        return (root, query, cb) ->
                region == null ? null : cb.like(cb.lower(root.get("region")), "%" + region.toLowerCase() + "%");
    }

    public static Specification<Supplier> createdBetween(LocalDateTime after, LocalDateTime before) {
        return (root, query, cb) -> {
            if (after == null && before == null) return null;
            if (after != null && before != null)
                return cb.between(root.get("createdAt"), after, before);
            if (after != null)
                return cb.greaterThanOrEqualTo(root.get("createdAt"), after);
            return cb.lessThanOrEqualTo(root.get("createdAt"), before);
        };
    }
}