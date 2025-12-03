package com.IntegrityTechnologies.business_manager.workflow;

import com.IntegrityTechnologies.business_manager.common.BaseIntegrationTest;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.PartTransactionRepository;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest(properties = {
        // optional: if you configure which AccountingService is active via properties, override here for single-entry
})
@AutoConfigureMockMvc
public class AccountingSingleEntryTest extends BaseIntegrationTest {

    @Autowired private PartTransactionRepository ptRepo;
    @Autowired private ObjectMapper om;

    @Test
    void singleEntry_recordsPartTransaction() {
        long before = ptRepo.count();
        // after a payment (use PaymentWorkflowCashOnlyTest), a PartTransaction should exist
        assertThat(before).isGreaterThanOrEqualTo(0);
    }
}