//package com.IntegrityTechnologies.business_manager.workflow;
//
//import com.IntegrityTechnologies.business_manager.common.BaseIntegrationTest;
//import com.fasterxml.jackson.databind.JsonNode;
//import com.fasterxml.jackson.databind.ObjectMapper;
//import org.junit.jupiter.api.*;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.http.MediaType;
//import org.springframework.test.web.servlet.MockMvc;
//
//import java.util.UUID;
//
//import static org.assertj.core.api.Assertions.assertThat;
//import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
//import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;
//
//@SpringBootTest
//@AutoConfigureMockMvc
//@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
//public class SalesWorkflowTest extends BaseIntegrationTest {
//    private static UUID SALE_ID;
//
//    @Test @Order(1)
//    void createSale_reservesStock_and_createsCustomer() throws Exception {
//        String req = """
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
//                      "name":"John Buyer",
//                      "phoneNumbers":["0723999000"],
//                      "emailAddresses":["john.buyer@example.com"]
//                    }
//                  ]
//                }
//                """;
//
//        String json = mvc.perform(post("/api/sales")
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(req))
//                .andExpect(status().isOk())
//                .andReturn().getResponse().getContentAsString();
//
//        JsonNode node = om.readTree(json);
//        SALE_ID = UUID.fromString(node.get("saleId").asText());
//        assertThat(SALE_ID).isNotNull();
//    }
//
//    @Test @Order(2)
//    void getSale_and_receipt() throws Exception {
//        mvc.perform(get("/api/sales/" + SALE_ID))
//                .andExpect(status().isOk())
//                .andExpect(jsonPath("$.saleId").value(SALE_ID.toString()));
//
//        // receipt PDF endpoint returns bytes (200) — check status
//        mvc.perform(get("/api/sales/" + SALE_ID + "/receipt"))
//                .andExpect(status().isOk());
//    }
//
//    @Test @Order(3)
//    void listSales() throws Exception {
//        mvc.perform(get("/api/sales")
//                        .param("page", "0").param("size","10"))
//                .andExpect(status().isOk())
//                .andExpect(jsonPath("$.content").isArray());
//    }
//
//    @Test @Order(4)
//    void cancelAndUpdateBeforePayment() throws Exception {
//        // update sale: change nothing substantial but test endpoint
//        String updateReq = """
//                {
//                   "items":[
//                     {
//                       "productId":"88acd47d-c24b-4f6c-a1cd-27219cdf3405",
//                       "branchId":"0c951da7-6f90-491b-8a90-dec3eccd5cd9",
//                       "unitPrice":25000,
//                       "quantity":1
//                     }
//                   ],
//                   "customerIdentifiers":[
//                     {
//                       "name":"John Buyer",
//                       "phoneNumbers":["0723999000"]
//                     }
//                   ]
//                }
//                """;
//        mvc.perform(put("/api/sales/" + SALE_ID)
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(updateReq))
//                .andExpect(status().isOk());
//
//        // cancel
//        mvc.perform(post("/api/sales/" + SALE_ID + "/cancel"))
//                .andExpect(status().isOk());
//    }
//}