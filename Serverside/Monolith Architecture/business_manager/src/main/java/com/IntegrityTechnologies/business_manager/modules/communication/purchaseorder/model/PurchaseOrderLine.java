//package com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.config;
//
//import jakarta.persistence.*;
//import lombok.*;
//
//import java.math.BigDecimal;
//import java.util.UUID;
//
//@Entity
//@Table(name = "purchase_order_lines")
//@Data
//@NoArgsConstructor
//@AllArgsConstructor
//@Builder
//public class PurchaseOrderLine {
//
//    @Id
//    @GeneratedValue(strategy = GenerationType.IDENTITY)
//    private Long id;
//
//    @Column(columnDefinition = "BINARY(16)")
//    private UUID productId;
//
//    private String productName;
//
//    private Integer quantity;
//
//    private BigDecimal unitPrice;
//
//    private BigDecimal lineTotal;
//}