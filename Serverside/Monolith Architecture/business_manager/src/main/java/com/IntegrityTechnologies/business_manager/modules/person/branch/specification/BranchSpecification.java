package com.IntegrityTechnologies.business_manager.modules.person.branch.specification;

import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.BranchQueryDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import org.springframework.data.jpa.domain.Specification;

import java.util.UUID;

public class BranchSpecification {

    private BranchSpecification() {
    }

    public static Specification<Branch> build(
            UUID tenantId,
            BranchQueryDTO query
    ) {

        Specification<Branch> spec =
                tenantFilter(tenantId);

        spec = spec.and(
                deletedFilter(query.getDeleted())
        );

        Specification<Branch> searchSpec =
                searchFilter(query.getSearch());

        if (searchSpec != null) {
            spec = spec.and(searchSpec);
        }

        return spec;
    }

    private static Specification<Branch> tenantFilter(
            UUID tenantId
    ) {

        return (root, cq, cb) ->
                cb.equal(
                        root.get("tenantId"),
                        tenantId
                );
    }

    private static Specification<Branch> deletedFilter(
            Boolean deleted
    ) {

        if (Boolean.TRUE.equals(deleted)) {

            return (root, cq, cb) ->
                    cb.conjunction();

        }

        return (root, cq, cb) ->
                cb.isFalse(
                        root.get("deleted")
                );
    }

    private static Specification<Branch> searchFilter(
            String search
    ) {

        if (search == null || search.isBlank()) {
            return null;
        }

        String pattern =
                "%" + search.trim().toLowerCase() + "%";

        return (root, cq, cb) ->
                cb.or(
                        cb.like(
                                cb.lower(root.get("name")),
                                pattern
                        ),
                        cb.like(
                                cb.lower(root.get("branchCode")),
                                pattern
                        ),
                        cb.like(
                                cb.lower(
                                        cb.coalesce(
                                                root.get("location"),
                                                ""
                                        )
                                ),
                                pattern
                        )
                );
    }
}