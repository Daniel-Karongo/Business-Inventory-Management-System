//package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;
//
//import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
//import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
//import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
//import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
//import lombok.RequiredArgsConstructor;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.stereotype.Service;
//import org.springframework.transaction.annotation.Transactional;
//
//import java.util.List;
//
//@Service
//@RequiredArgsConstructor
//@Slf4j
//public class ProductBarcodeMigrationService {
//
//    private final ProductRepository productRepository;
//    private final ProductVariantRepository variantRepository;
//    private final VariantBarcodeService barcodeService;
//
//    /**
//     * Copies product.barcode → product_variant.barcode
//     * Does NOT delete product barcode.
//     */
//    @Transactional
//    public void migrateProductBarcodesToVariants() {
//
//        List<Product> products = productRepository.findAll();
//
//        for (Product product : products) {
//
//            if (product.getBarcode() == null || product.getBarcode().isBlank()) {
//                continue;
//            }
//
//            List<ProductVariant> variants =
//                    variantRepository.findByProduct_Id(product.getId());
//
//            if (variants.isEmpty()) {
//                log.warn("Product {} has barcode but no variants — skipping", product.getId());
//                continue;
//            }
//
//            ProductVariant target = variants.get(0); // DEFAULT / first variant
//
//            if (target.getBarcode() != null && !target.getBarcode().isBlank()) {
//                continue; // already migrated
//            }
//
//            target.setBarcode(product.getBarcode());
//
//            // generate barcode image safely
//            barcodeService.generateBarcodeIfMissing(target.getId());
//
//            variantRepository.save(target);
//
//            log.info(
//                    "Migrated barcode {} from product {} → variant {}",
//                    product.getBarcode(),
//                    product.getId(),
//                    target.getId()
//            );
//        }
//    }
//}