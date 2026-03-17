//package com.IntegrityTechnologies.business_manager.config;
//
//import jakarta.annotation.PostConstruct;
//import lombok.RequiredArgsConstructor;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.stereotype.Component;
//
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.util.Comparator;
//
//@Slf4j
//@Component
//@RequiredArgsConstructor
//public class UploadsInitializer {
//
//    private final FileStorageService storage;
//
//    @PostConstruct
//    public void init() {
//
//        try {
//
//            Path uploadsRoot = storage.userRoot().getParent();
//
//            Files.walk(uploadsRoot)
//                    .sorted(Comparator.reverseOrder())
//                    .forEach(storage::secure);
//
//            log.info("Uploads directory secured");
//
//        } catch (Exception e) {
//            throw new RuntimeException("Uploads initialization failed", e);
//        }
//
//    }
//}