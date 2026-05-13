package com.IntegrityTechnologies.business_manager.modules.person.customer.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "customer_payment_history",
        indexes = {
                @Index(name = "idx_cph_customer", columnList = "customerId"),
                @Index(name = "idx_cph_tenant_branch", columnList = "tenant_id,branch_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class CustomerPaymentHistory extends BranchAwareEntity {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(columnDefinition = "BINARY(16)", nullable = false)
    private UUID customerId;

    @Column(columnDefinition = "BINARY(16)", nullable = false)
    private UUID paymentId;

    @Column(nullable = false)
    private BigDecimal amount;

    @Column(nullable = false)
    private LocalDateTime timestamp;

    private String note;

    @Version
    @Column(name = "version", nullable = false)
    private Long version;
}