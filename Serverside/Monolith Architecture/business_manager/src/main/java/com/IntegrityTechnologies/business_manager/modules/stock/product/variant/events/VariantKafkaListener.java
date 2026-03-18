package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.events;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@ConditionalOnProperty(
        name = "spring.kafka.enabled",
        havingValue = "true"
)
public class VariantKafkaListener {

    private final VariantEventHandler handler;

    @KafkaListener(
            topics = "variant-barcode",
            groupId = "variant-processor",
            containerFactory = "kafkaListenerContainerFactory"
    )
    public void handleBarcode(VariantBarcodeRequestedEvent event) {
        handler.handle(event);
    }

    @KafkaListener(
            topics = "variant-image",
            groupId = "variant-processor",
            containerFactory = "kafkaListenerContainerFactory"
    )
    public void handleImage(VariantImageUploadRequestedEvent event) {
        handler.handle(event);
    }

    @KafkaListener(
            topics = "variant-pdf",
            groupId = "variant-processor",
            containerFactory = "kafkaListenerContainerFactory"
    )
    public void handlePdf(VariantBarcodePdfRequestedEvent event) {
        handler.handle(event);
    }
}