package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Entity
@Getter
@Setter
public class ProcessedKafkaEvent {

    @Id
    private UUID eventId;
}