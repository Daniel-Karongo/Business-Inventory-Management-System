package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.specification;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.SupplierPaymentSearchRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.model.SupplierPayment;
import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.Predicate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class SupplierPaymentSpecification {

    public static Specification<SupplierPayment> search(
            UUID tenantId,
            UUID branchId,
            SupplierPaymentSearchRequest request
    ) {
        return (root, query, cb) -> {

            List<Predicate> predicates =
                    new ArrayList<>();

            predicates.add(
                    cb.equal(
                            root.get("tenantId"),
                            tenantId
                    )
            );

            predicates.add(
                    cb.equal(
                            root.get("branchId"),
                            branchId
                    )
            );

            if (request.getSupplierId() != null) {
                predicates.add(
                        cb.equal(
                                root.get("supplierId"),
                                request.getSupplierId()
                        )
                );
            }

            if (request.getStatus() != null) {
                predicates.add(
                        cb.equal(
                                root.get("status"),
                                request.getStatus()
                        )
                );
            }

            if (request.getMethod() != null) {
                predicates.add(
                        cb.equal(
                                root.get("method"),
                                request.getMethod()
                        )
                );
            }

            if (request.getFundingAccountId() != null) {
                predicates.add(
                        cb.equal(
                                root.get("fundingAccountId"),
                                request.getFundingAccountId()
                        )
                );
            }

            if (
                    Boolean.TRUE.equals(
                            request.getUnappliedOnly()
                    )
            ) {
                predicates.add(
                        cb.greaterThan(
                                root.get("unappliedAmount"),
                                BigDecimal.ZERO
                        )
                );
            }

            if (request.getReversed() != null) {
                predicates.add(
                        cb.equal(
                                root.get("reversed"),
                                request.getReversed()
                        )
                );
            }

            if (request.getFromDate() != null) {
                predicates.add(
                        cb.greaterThanOrEqualTo(
                                root.get("paymentDate"),
                                request.getFromDate()
                        )
                );
            }

            if (request.getToDate() != null) {
                predicates.add(
                        cb.lessThanOrEqualTo(
                                root.get("paymentDate"),
                                request.getToDate()
                        )
                );
            }

            if (
                    request.getReference() != null
                            && !request.getReference().isBlank()
            ) {
                predicates.add(
                        cb.like(
                                cb.lower(
                                        root.get("reference")
                                ),
                                "%"
                                        + request.getReference()
                                        .toLowerCase()
                                        + "%"
                        )
                );
            }

            return cb.and(
                    predicates.toArray(
                            Predicate[]::new
                    )
            );
        };
    }
}