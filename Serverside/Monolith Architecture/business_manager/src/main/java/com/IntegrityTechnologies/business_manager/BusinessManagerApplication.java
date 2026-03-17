package com.IntegrityTechnologies.business_manager;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.data.web.config.EnableSpringDataWebSupport;
import org.springframework.kafka.annotation.EnableKafka;

@SpringBootApplication
@EnableAspectJAutoProxy
@EnableKafka
@EnableSpringDataWebSupport(pageSerializationMode = EnableSpringDataWebSupport.PageSerializationMode.VIA_DTO)
public class BusinessManagerApplication {

	public static void main(String[] args) {
		SpringApplication.run(BusinessManagerApplication.class, args);
	}

}
