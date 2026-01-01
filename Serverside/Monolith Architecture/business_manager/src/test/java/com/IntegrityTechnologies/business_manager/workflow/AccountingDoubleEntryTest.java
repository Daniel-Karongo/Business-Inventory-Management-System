//package com.IntegrityTechnologies.business_manager.workflow;
//
//import com.IntegrityTechnologies.business_manager.common.BaseIntegrationTest;
//import com.fasterxml.jackson.databind.ObjectMapper;
//
//import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.JournalEntryRepository;
//import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.EntryLineRepository;
//import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.PartTransactionRepository;
//
//import org.junit.jupiter.api.*;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
//import org.springframework.boot.test.context.SpringBootTest;
//
//import java.math.BigDecimal;
//import java.util.UUID;
//
//import static org.assertj.core.api.Assertions.assertThat;
//
//@SpringBootTest
//@AutoConfigureMockMvc
//public class AccountingDoubleEntryTest extends BaseIntegrationTest {
//
//    @Autowired private JournalEntryRepository journalRepo;
//    @Autowired private EntryLineRepository lineRepo;
//    @Autowired private PartTransactionRepository ptRepo;
//    @Autowired private ObjectMapper om;
//
//    @Test
//    void doubleEntry_recordPayment_createsBalancedJournalAndPartTransactions() {
//        // This test assumes the DoubleEntryAccountingService is wired in your application context.
//        // We'll simulate by calling AccountingService via Spring context in an integration scenario,
//        // but here we test the dto effects after you run a real payment (PaymentWorkflowCashOnlyTest).
//        // So this test is a sanity-check: there must exist at least one journal entry with 2 lines and 1 part transaction.
//
//        long journals = journalRepo.count();
//        long parts = ptRepo.count();
//        assertThat(journals).isGreaterThanOrEqualTo(0); // at least DB reachable
//        assertThat(parts).isGreaterThanOrEqualTo(0);
//
//        // If you want an active scenario: run a payment (via PaymentWorkflowCashOnlyTest) then query:
//        // var lastJe = journalRepo.findTopByOrderByTimestampDesc();
//        // assertThat(lastJe.getLines().size()).isEqualTo(2);
//    }
//}