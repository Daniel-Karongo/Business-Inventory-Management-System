//package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;
//
//import org.springframework.cache.CacheManager;
//import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//
//@Configuration
//public class FinancialReportCacheConfig {
//
//    @Bean
//    public CacheManager cacheManager() {
//
//        return new ConcurrentMapCacheManager(
//                "trialBalance",
//                "profitLoss",
//                "balanceSheet",
//                "cashFlow",
//                "accountsReceivable",
//                "accountsPayable"
//        );
//    }
//}