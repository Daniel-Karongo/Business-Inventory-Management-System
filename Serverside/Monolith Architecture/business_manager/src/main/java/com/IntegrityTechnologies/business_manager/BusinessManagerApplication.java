package com.IntegrityTechnologies.business_manager;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.kafka.annotation.EnableKafka;

@SpringBootApplication
@EnableAspectJAutoProxy
@EnableKafka
public class BusinessManagerApplication {

	public static void main(String[] args) {
		SpringApplication.run(BusinessManagerApplication.class, args);
	}

}
