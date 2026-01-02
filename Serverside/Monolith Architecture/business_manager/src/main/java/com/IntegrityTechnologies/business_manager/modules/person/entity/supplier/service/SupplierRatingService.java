//package com.IntegrityTechnologies.business_manager.modules.supplier.service;
//
//import com.IntegrityTechnologies.business_manager.modules.stock.config.product.Product;
//import com.IntegrityTechnologies.business_manager.modules.person.entity.config.supplier.Supplier;
//import com.IntegrityTechnologies.business_manager.modules.person.entity.dto.supplier.SupplierRepository;
//import lombok.RequiredArgsConstructor;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.stereotype.Service;
//
//import java.math.BigDecimal;
//import java.math.RoundingMode;
//import java.time.Duration;
//import java.time.LocalDateTime;
//
//@Service
//@RequiredArgsConstructor
//@Slf4j
//public class SupplierRatingService {
//
//    private final SupplierRepository supplierRepository;
//
//    /**
//     * Update supplier rating based on product delivery performance and quality feedback.
//     *
//     * @param supplier      Supplier entity
//     * @param deliveryDate  Date of actual delivery
//     * @param expectedDate  Expected delivery date
//     * @param qualityScore  Quality score (0.0–5.0)
//     */
//    public void updateSupplierRating(Supplier supplier,
//                                     LocalDateTime deliveryDate,
//                                     LocalDateTime expectedDate,
//                                     double qualityScore) {
//        if (supplier == null) return;
//
//        double punctualityScore = calculatePunctualityScore(deliveryDate, expectedDate);
//
//        // Use existing rating or neutral default
//        double oldRating = supplier.getRating() != null ? supplier.getRating() : 3.0;
//
//        // Weighted average: 70% old rating, 30% new performance
//        double newRating = (oldRating * 0.7) + ((punctualityScore + qualityScore) / 2 * 0.3);
//
//        // Clamp and round
//        double normalized = BigDecimal.valueOf(Math.min(5.0, Math.max(0.0, newRating)))
//                .setScale(2, RoundingMode.HALF_UP)
//                .doubleValue();
//
//        supplier.setRating(normalized);
//        supplierRepository.save(supplier);
//
//        log.info("⭐ Updated supplier '{}' rating to {}", supplier.getName(), normalized);
//    }
//
//    /**
//     * Simple restock-based rating update
//     */
//    public void onProductRestock(Product product, double qualityScore) {
//        if (product.getLastSuppliedBy() == null) return;
//
//        Supplier supplier = product.getLastSuppliedBy();
//        double oldRating = supplier.getRating() != null ? supplier.getRating() : 3.0;
//
//        double newRating = (oldRating * 0.8) + (qualityScore * 0.2);
//        double clamped = BigDecimal.valueOf(Math.min(5.0, Math.max(0.0, newRating)))
//                .setScale(2, RoundingMode.HALF_UP)
//                .doubleValue();
//
//        supplier.setRating(clamped);
//        supplierRepository.save(supplier);
//
//        log.info("🔁 Auto-updated rating for supplier '{}' after restock to {}", supplier.getName(), clamped);
//    }
//
//    private double calculatePunctualityScore(LocalDateTime delivery, LocalDateTime expected) {
//        if (delivery == null || expected == null) return 3.0; // neutral
//
//        long daysDiff = Duration.between(expected, delivery).toDays();
//        if (daysDiff <= 0) return 5.0;
//        if (daysDiff == 1) return 4.0;
//        if (daysDiff <= 3) return 3.0;
//        if (daysDiff <= 7) return 2.0;
//        return 1.0;
//    }
//}