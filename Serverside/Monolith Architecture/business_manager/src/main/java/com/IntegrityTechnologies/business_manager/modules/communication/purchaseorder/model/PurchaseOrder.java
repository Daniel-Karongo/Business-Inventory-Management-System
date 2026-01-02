//package com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.config;
//
//import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.config.Supplier;
//import jakarta.persistence.*;
//import lombok.*;
//import java.time.LocalDateTime;
//import java.util.List;
//import java.util.UUID;
//
//@Entity
//@Table(name = "purchase_orders")
//@Data
//@NoArgsConstructor
//@AllArgsConstructor
//@Builder
//public class PurchaseOrder {
//
//    @Id
//    @GeneratedValue
//    @Column(columnDefinition = "BINARY(16)")
//    private UUID id;
//
//    private String poNumber;
//
//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "supplier_id")
//    private Supplier supplier;
//
//    private LocalDateTime createdAt;
//    private String createdBy;
//
//    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
//    @JoinColumn(name = "purchase_order_id")
//    private List<PurchaseOrderLine> lines;
//
//    @Enumerated(EnumType.STRING)
//    private Status status;
//
//    public enum Status { DRAFT, SENT, RECEIVED, CANCELLED }
//}