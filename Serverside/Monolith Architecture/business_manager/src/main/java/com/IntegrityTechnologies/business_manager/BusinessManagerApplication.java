package com.IntegrityTechnologies.business_manager;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

@SpringBootApplication
@EnableAspectJAutoProxy
public class BusinessManagerApplication {

	public static void main(String[] args) {
		SpringApplication.run(BusinessManagerApplication.class, args);
	}

}
