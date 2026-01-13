package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.service;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariant;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository.ProductVariantRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
@RequiredArgsConstructor
@Slf4j
public class VariantBarcodeBackfillService {

    private final ProductVariantRepository variantRepo;
    private final VariantBarcodeService barcodeService;

    @Transactional
    public void backfillBarcodesAndImages() {

        List<ProductVariant> variants = variantRepo.findAll();
        Set<String> used = new HashSet<>();

        for (ProductVariant v : variants) {

            /* =============================
               BARCODE
               ============================= */

            if (v.getBarcode() == null || v.getBarcode().isBlank()) {

                String barcode;
                do {
                    barcode = barcodeService.autoGenerateBarcode();
                } while (
                        used.contains(barcode) ||
                                variantRepo.findByBarcode(barcode).isPresent()
                );

                v.setBarcode(barcode);
                used.add(barcode);

                log.info("Generated barcode {} for variant {}", barcode, v.getId());
            }

            /* =============================
               BARCODE IMAGE
               ============================= */

            if (v.getBarcodeImagePath() == null || v.getBarcodeImagePath().isBlank()) {

                Path imagePath =
                        barcodeService.generateBarcodeImage(
                                v.getBarcode(), v
                        );

                v.setBarcodeImagePath(imagePath.toString());

                log.info(
                        "Generated barcode image for variant {} at {}",
                        v.getId(), imagePath
                );
            }

            variantRepo.save(v);
        }

        log.info("✅ Variant barcode + image backfill completed");
    }
}