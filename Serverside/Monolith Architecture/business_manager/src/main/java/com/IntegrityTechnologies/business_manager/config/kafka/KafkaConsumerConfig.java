package com.IntegrityTechnologies.business_manager.config.kafka;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.kafka.support.serializer.JsonDeserializer;

import java.util.HashMap;
import java.util.Map;

@ConditionalOnProperty(
        name = "spring.kafka.enabled",
        havingValue = "true",
        matchIfMissing = false
)
@Configuration
public class KafkaConsumerConfig {

    @Bean
    public ConsumerFactory<String, JournalPostedEvent> consumerFactory() {

        JsonDeserializer<JournalPostedEvent> deserializer =
                new JsonDeserializer<>(JournalPostedEvent.class);

        deserializer.addTrustedPackages("*");

        Map<String, Object> config = new HashMap<>();

        config.put(
                ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG,
                "localhost:9092"
        );

        config.put(
                ConsumerConfig.GROUP_ID_CONFIG,
                "accounting-engine"
        );

        config.put(
                ConsumerConfig.AUTO_OFFSET_RESET_CONFIG,
                "earliest"
        );

        return new DefaultKafkaConsumerFactory<>(
                config,
                new org.apache.kafka.common.serialization.StringDeserializer(),
                deserializer
        );
    }

    @Bean
    public ConcurrentKafkaListenerContainerFactory<String, JournalPostedEvent>
    kafkaListenerContainerFactory() {

        ConcurrentKafkaListenerContainerFactory<String, JournalPostedEvent> factory =
                new ConcurrentKafkaListenerContainerFactory<>();

        factory.setConsumerFactory(consumerFactory());

        factory.setConcurrency(3);

        return factory;
    }
}