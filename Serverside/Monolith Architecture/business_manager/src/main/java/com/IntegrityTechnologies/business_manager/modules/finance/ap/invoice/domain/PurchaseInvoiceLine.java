package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.domain;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.model.ProductVariant;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.UuidGenerator;

import java.math.BigDecimal;
import java.util.UUID;

@Entity
@Table(
        name = "purchase_invoice_lines",
        indexes = {
                @Index(
                        name = "idx_purchase_invoice_line_invoice",
                        columnList = "purchase_invoice_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class PurchaseInvoiceLine {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @UuidGenerator
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(
            name = "purchase_invoice_id",
            nullable = false
    )
    private PurchaseInvoice purchaseInvoice;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id")
    private Product product;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_variant_id")
    private ProductVariant productVariant;

    @Column(nullable = false)
    private String productNameSnapshot;

    @Column(nullable = false)
    private String productSkuSnapshot;

    @Column(nullable = false)
    private String variantNameSnapshot;

    @Column(nullable = false)
    private Long quantity;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal unitCost;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal discountAmount = BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    @Builder.Default
    private BigDecimal vatAmount = BigDecimal.ZERO;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal lineSubtotal;

    @Column(nullable = false, precision = 19, scale = 2)
    private BigDecimal lineTotal;
}