package com.IntegrityTechnologies.business_manager.config.jackson;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.Module;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.yubico.webauthn.data.ByteArray;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

import java.io.IOException;

@Configuration
public class JacksonConfig {

    @Bean
    public SimpleModule webAuthnJacksonModule() {
        SimpleModule module = new SimpleModule();

        module.addSerializer(ByteArray.class, new JsonSerializer<>() {
            @Override
            public void serialize(ByteArray value, JsonGenerator gen, SerializerProvider serializers)
                    throws IOException {
                gen.writeString(value.getBase64Url());
            }
        });

        return module;
    }

    @Bean
    public ObjectMapper objectMapper(SimpleModule webAuthnJacksonModule) {
        ObjectMapper mapper = new ObjectMapper();

        mapper.registerModule(new Jdk8Module());
        mapper.registerModule(new JavaTimeModule());
        mapper.registerModule(webAuthnJacksonModule);

        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

        return mapper;
    }

    @Bean
    public MappingJackson2HttpMessageConverter mappingJackson2HttpMessageConverter(
            ObjectMapper objectMapper
    ) {
        return new MappingJackson2HttpMessageConverter(objectMapper);
    }
}