package com.IntegrityTechnologies.business_manager.config.kafka;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.AccountingPeriodClosedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantBarcodePdfRequestedEvent;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantBarcodeRequestedEvent;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.events.VariantImageUploadRequestedEvent;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.NestedExceptionUtils;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.SQLIntegrityConstraintViolationException;
import java.time.LocalDateTime;

@Slf4j
@Service
@RequiredArgsConstructor
public class OutboxEventExecutor {

    private final EventOutboxRepository outboxRepo;
    private final DomainEventPublisher publisher;
    private final ObjectMapper mapper;

    @Transactional
    public void processEvent(EventOutbox e) {

        try {

            TenantContext.setTenantId(e.getTenantId());

            Object event = deserialize(e);

            publisher.publish(e.getEventType(), event);

            // SUCCESS
            e.setProcessed(true);
            e.setProcessedAt(LocalDateTime.now());
            e.setRetryCount(0);

            outboxRepo.save(e);

        } catch (DataIntegrityViolationException ex) {

            Throwable root =
                    NestedExceptionUtils.getMostSpecificCause(ex);

            if (root instanceof SQLIntegrityConstraintViolationException
                    && root.getMessage() != null
                    && root.getMessage().contains("processed_kafka_events")) {

                log.debug(
                        "Duplicate consumer execution suppressed for event {}",
                        e.getId()
                );

                e.setProcessed(true);
                e.setProcessedAt(LocalDateTime.now());

                outboxRepo.save(e);

                return;
            }

            handleFailure(e, ex);

        } catch (Exception ex) {

            handleFailure(e, ex);
        } finally {
            TenantContext.clear();
        }
    }

    private void handleFailure(EventOutbox e, Exception ex) {

        e.setRetryCount(e.getRetryCount() + 1);

        if (e.getRetryCount() > 10) {

            e.setFailed(true);
            e.setFailureReason(ex.getMessage());
            e.setProcessed(true);

            log.error(
                    "DLQ: Event {} moved to failed state",
                    e.getId(),
                    ex
            );

        } else {

            if (e.getRetryCount() <= 3
                    || e.getRetryCount() % 10 == 0) {

                log.warn(
                        "Retry {} for event {} type={}",
                        e.getRetryCount(),
                        e.getId(),
                        e.getEventType()
                );
            }
        }

        outboxRepo.save(e);
    }

    private Object deserialize(EventOutbox e) {

        try {

            return switch (e.getEventType()) {

                case "JOURNAL_POSTED" -> mapper.readValue(e.getPayload(), JournalPostedEvent.class);

                case "ACCOUNTING_PERIOD_CLOSED" -> mapper.readValue(e.getPayload(), AccountingPeriodClosedEvent.class);

                case "VARIANT_BARCODE_REQUESTED" ->
                        mapper.readValue(e.getPayload(), VariantBarcodeRequestedEvent.class);

                case "VARIANT_IMAGE_UPLOAD_REQUESTED" ->
                        mapper.readValue(e.getPayload(), VariantImageUploadRequestedEvent.class);

                case "VARIANT_BARCODE_PDF_REQUESTED" ->
                        mapper.readValue(e.getPayload(), VariantBarcodePdfRequestedEvent.class);

                default -> mapper.readTree(e.getPayload());
            };

        } catch (Exception ex) {

            throw new RuntimeException(
                    "Failed to deserialize outbox event " + e.getId(),
                    ex
            );
        }
    }
}