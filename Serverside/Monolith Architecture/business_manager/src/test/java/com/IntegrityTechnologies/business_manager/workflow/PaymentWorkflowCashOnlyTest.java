//package com.IntegrityTechnologies.business_manager.workflow;
//
//import com.IntegrityTechnologies.business_manager.common.BaseIntegrationTest;
//import com.fasterxml.jackson.databind.JsonNode;
//import com.fasterxml.jackson.databind.ObjectMapper;
//
//import org.junit.jupiter.api.*;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.http.MediaType;
//import org.springframework.test.web.servlet.MockMvc;
//
//import java.math.BigDecimal;
//import java.util.UUID;
//
//import static org.assertj.core.api.Assertions.assertThat;
//import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
//import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
//import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
//
//@SpringBootTest
//@AutoConfigureMockMvc
//@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
//public class PaymentWorkflowCashOnlyTest extends BaseIntegrationTest {
//    private static UUID saleId;
//    private static UUID paymentId;
//
//    @Test @Order(1)
//    void prepareSale_for_payment() throws Exception {
//        String saleReq = """
//                {
//                  "items":[
//                    {
//                      "productId":"88acd47d-c24b-4f6c-a1cd-27219cdf3405",
//                      "branchId":"0c951da7-6f90-491b-8a90-dec3eccd5cd9",
//                      "unitPrice":25000,
//                      "quantity":1
//                    }
//                  ],
//                  "customerIdentifiers":[
//                    {
//                      "name":"Cash Buyer",
//                      "phoneNumbers":["0723001111"]
//                    }
//                  ]
//                }
//                """;
//
//        String json = mvc.perform(post("/api/sales")
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(saleReq))
//                .andExpect(status().isOk())
//                .andReturn().getResponse().getContentAsString();
//
//        JsonNode n = om.readTree(json);
//        saleId = UUID.fromString(n.get("saleId").asText());
//        assertThat(saleId).isNotNull();
//    }
//
//    @Test @Order(2)
//    void payCash_and_verify_inventory_change() throws Exception {
//        String payReq = """
//                {
//                  "saleId":"%s",
//                  "amount":25000,
//                  "method":"CASH",
//                  "note":"Cash payment from test"
//                }
//                """.formatted(saleId);
//
//        String json = mvc.perform(post("/api/payments")
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(payReq))
//                .andExpect(status().isOk())
//                .andReturn().getResponse().getContentAsString();
//
//        JsonNode p = om.readTree(json);
//        paymentId = UUID.fromString(p.get("paymentId").asText());
//        assertThat(paymentId).isNotNull();
//
//        // Reconcile quick check: call reconcile endpoint for year range
//        mvc.perform(get("/api/payments/reconcile")
//                        .param("from","2025-01-01")
//                        .param("to","2025-12-31"))
//                .andExpect(status().isOk());
//    }
//
//    @Test @Order(3)
//    void refundCashPayment() throws Exception {
//        mvc.perform(post("/api/payments/" + paymentId + "/refund"))
//                .andExpect(status().isOk());
//    }
//}