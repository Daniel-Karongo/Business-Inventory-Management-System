package com.IntegrityTechnologies.business_manager.modules.finance.sales.model;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.model.Payment;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Entity
@Table(
        name = "sales",
        indexes = {
                @Index(name = "idx_sales_created_at", columnList = "createdAt"),
                @Index(name = "idx_sales_tenant_branch", columnList = "tenant_id, branch_id")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class Sale extends BranchAwareEntity {

    @Id
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(unique = true)
    private String receiptNo;

    private String createdBy;

    @Column(precision = 19, scale = 2)
    private BigDecimal totalAmount;

    @Column(precision = 19, scale = 2)
    private BigDecimal totalTax;

    @Column(precision = 19, scale = 2)
    private BigDecimal totalDiscount;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "sale_id")
    @Builder.Default
    private List<SaleLineItem> lineItems = new ArrayList<>();

    @OneToMany(mappedBy = "sale", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private List<Payment> payments = new ArrayList<>();

    @Column(columnDefinition = "BINARY(16)")
    private UUID customerId;

    @Enumerated(EnumType.STRING)
    private SaleStatus status;

    @Version
    private Long version;

    @Column(precision = 19, scale = 2)
    private BigDecimal costOfGoodsSold;

    public enum SaleStatus {
        CREATED, COMPLETED, CANCELLED, REFUNDED
    }
}