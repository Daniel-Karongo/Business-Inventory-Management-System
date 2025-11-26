package com.IntegrityTechnologies.business_manager.modules.supplier.service;

import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import org.springframework.data.jpa.domain.Specification;

import java.time.LocalDateTime;
import java.util.List;

public class SupplierSpecification {

    public static Specification<Supplier> inCategories(List<Long> categoryIds) {
        if (categoryIds == null || categoryIds.isEmpty()) return null;
        return (root, query, cb) -> {
            Join<Object, Object> join = root.join("categories", JoinType.INNER);
            return join.get("id").in(categoryIds);
        };
    }

    public static Specification<Supplier> hasNameLike(String name) {
        if (name == null || name.isBlank()) return null;
        return (root, query, cb) -> cb.like(cb.lower(root.get("name")), "%" + name.toLowerCase() + "%");
    }

    public static Specification<Supplier> hasEmail(String email) {
        if (email == null || email.isBlank()) return null;
        return (root, query, cb) -> {
            Join<String, String> join = root.join("email", JoinType.LEFT);
            return cb.equal(cb.lower(join), email.toLowerCase());
        };
    }

    public static Specification<Supplier> hasPhone(String phone) {
        if (phone == null || phone.isBlank()) return null;
        return (root, query, cb) -> {
            Join<Object, Object> join = root.join("phoneNumber", JoinType.LEFT);
            return cb.equal(join, phone);
        };
    }

    public static Specification<Supplier> hasMinRating(Double minRating) {
        if (minRating == null) return null;
        return (root, query, cb) -> cb.greaterThanOrEqualTo(root.get("rating"), minRating);
    }

    public static Specification<Supplier> hasDeletedFlag(Boolean deleted) {
        if (deleted == null) {
            // Return everything → no filter added
            return null;
        }
        return (root, query, cb) -> cb.equal(root.get("deleted"), deleted);
    }

    public static Specification<Supplier> fromRegion(String region) {
        if (region == null || region.isBlank()) return null;
        return (root, query, cb) -> cb.equal(cb.lower(root.get("region")), region.toLowerCase());
    }

    public static Specification<Supplier> createdBetween(LocalDateTime after, LocalDateTime before) {
        if (after == null && before == null) return null;
        return (root, query, cb) -> {
            if (after != null && before != null) {
                return cb.between(root.get("createdAt"), after, before);
            } else if (after != null) {
                return cb.greaterThanOrEqualTo(root.get("createdAt"), after);
            } else {
                return cb.lessThanOrEqualTo(root.get("createdAt"), before);
            }
        };
    }
}