package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.VariantAsyncProcessor;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class VariantEventHandler {

    private final VariantAsyncProcessor processor;

    public void handle(VariantBarcodeRequestedEvent event) {
        processor.processBarcode(event);
    }

    public void handle(VariantImageUploadRequestedEvent event) {
        processor.processImage(event);
    }

    public void handle(VariantBarcodePdfRequestedEvent event) {
        processor.processPdf(event);
    }
}