package com.IntegrityTechnologies.business_manager.workflow;

import com.IntegrityTechnologies.business_manager.common.BaseIntegrationTest;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class FullDayRandomSimulationTest extends BaseIntegrationTest {
    // product/branch pairs from your inventory snapshot
    private static final List<Map<String,String>> PRODUCT_BRANCH = List.of(
            Map.of("product","88acd47d-c24b-4f6c-a1cd-27219cdf3405","branch","0c951da7-6f90-491b-8a90-dec3eccd5cd9"),
            Map.of("product","88acd47d-c24b-4f6c-a1cd-27219cdf3405","branch","ba8f46ab-ef8f-437d-a591-f48ece53dc04"),
            Map.of("product","f9d44c85-3bf3-4962-a23a-75f9748060a2","branch","0c951da7-6f90-491b-8a90-dec3eccd5cd9"),
            Map.of("product","f9d44c85-3bf3-4962-a23a-75f9748060a2","branch","ba8f46ab-ef8f-437d-a591-f48ece53dc04"),
            Map.of("product","d2a6a972-d97e-4af9-8282-6cceb5e6ad0a","branch","0c951da7-6f90-491b-8a90-dec3eccd5cd9"),
            Map.of("product","d2a6a972-d97e-4af9-8282-6cceb5e6ad0a","branch","ba8f46ab-ef8f-437d-a591-f48ece53dc04"),
            Map.of("product","dd52ac3c-b986-44b4-b8f2-7aab7a361e66","branch","0c951da7-6f90-491b-8a90-dec3eccd5cd9"),
            Map.of("product","dd52ac3c-b986-44b4-b8f2-7aab7a361e66","branch","ba8f46ab-ef8f-437d-a591-f48ece53dc04")
            // add more pairs from your snapshot if you like
    );

    @Test
    void simulateFullDay_randomized() throws Exception {
        long seed = Optional.ofNullable(System.getProperty("simulation.seed")).map(Long::parseLong).orElse(System.currentTimeMillis());
        int ops = Optional.ofNullable(System.getProperty("simulation.ops")).map(Integer::parseInt).orElse(100);

        Random rnd = new Random(seed);
        System.out.println("Simulation seed=" + seed + " ops=" + ops);

        List<UUID> createdSales = new ArrayList<>();
        List<UUID> createdPayments = new ArrayList<>();

        for (int i = 0; i < ops; i++) {
            int action = rnd.nextInt(100);
            if (action < 60) {
                // 60% create sale
                var pair = PRODUCT_BRANCH.get(rnd.nextInt(PRODUCT_BRANCH.size()));
                int qty = rnd.nextInt(3) + 1;
                int unitPrice = 10000 + rnd.nextInt(40000);

                String saleReq = """
                        {
                          "items":[
                            {
                              "productId":"%s",
                              "branchId":"%s",
                              "unitPrice":%d,
                              "quantity":%d
                            }
                          ],
                          "customerIdentifiers":[
                            {
                              "name":"Sim Customer %d",
                              "phoneNumbers":["07%d"],
                              "emailAddresses":["sim%d@example.com"]
                            }
                          ]
                        }
                        """.formatted(pair.get("product"), pair.get("branch"), unitPrice, qty, rnd.nextInt(100000), 700000000 + rnd.nextInt(99999), rnd.nextInt(100000));

                String res = mvc.perform(post("/api/sales")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(saleReq))
                        .andExpect(status().isOk())
                        .andReturn().getResponse().getContentAsString();

                JsonNode node = om.readTree(res);
                UUID sid = UUID.fromString(node.get("saleId").asText());
                createdSales.add(sid);
            } else if (action < 90 && !createdSales.isEmpty()) {
                // 30% pay a random sale (cash)
                UUID sid = createdSales.get(rnd.nextInt(createdSales.size()));
                // small chance to partially pay; mostly pay full
                String payReq = """
                        {
                          "saleId":"%s",
                          "amount":%d,
                          "method":"CASH",
                          "note":"Sim cash"
                        }
                        """.formatted(sid, 10000 + rnd.nextInt(50000));

                String resp = mvc.perform(post("/api/payments")
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(payReq))
                        .andExpect(status().isOk())
                        .andReturn().getResponse().getContentAsString();

                JsonNode p = om.readTree(resp);
                UUID pid = UUID.fromString(p.get("paymentId").asText());
                createdPayments.add(pid);
            } else if (!createdPayments.isEmpty()) {
                // 10% refund some payments
                UUID pid = createdPayments.get(rnd.nextInt(createdPayments.size()));
                mvc.perform(post("/api/payments/" + pid + "/refund"))
                        .andExpect(status().isOk());
            }

            // occasional reconciliation or inventory check
            if (i % 25 == 0) {
                mvc.perform(get("/api/payments/reconcile").param("from","2025-01-01").param("to","2025-12-31"))
                        .andExpect(status().isOk());
                mvc.perform(get("/api/inventory")).andExpect(status().isOk());
            }
        }

        // End of day assertions: at least one sale & payment should exist
        Assertions.assertFalse(createdSales.isEmpty(), "no sales created in simulation");
        // payments may be zero if randomness unlucky; we accept that but warn
        System.out.println("Simulation completed: sales=" + createdSales.size() + " payments=" + createdPayments.size());
    }
}