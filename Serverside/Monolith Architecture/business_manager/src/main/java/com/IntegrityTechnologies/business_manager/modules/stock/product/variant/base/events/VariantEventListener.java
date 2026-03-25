package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events;

import lombok.RequiredArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class VariantEventListener {

    private final VariantEventHandler handler;

    @EventListener
    public void onBarcodeRequested(VariantBarcodeRequestedEvent event) {
        handler.handle(event);
    }

    @EventListener
    public void onImageUploadRequested(VariantImageUploadRequestedEvent event) {
        handler.handle(event);
    }

    @EventListener
    public void onPdfRequested(VariantBarcodePdfRequestedEvent event) {
        handler.handle(event);
    }
}