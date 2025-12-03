package com.IntegrityTechnologies.business_manager.workflow;

import com.IntegrityTechnologies.business_manager.common.BaseIntegrationTest;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import java.util.UUID;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class InventoryWorkflowTest extends BaseIntegrationTest {

    private static final UUID PRODUCT = UUID.fromString("980847b6-3e76-405e-9712-108dbdd28721");
    private static final UUID BRANCH = UUID.fromString("ba8f46ab-ef8f-437d-a591-f48ece53dc04");

    @Test @Order(1)
    void testInventoryExists() throws Exception {
        mvc.perform(
                        authGet("/api/inventory/product")
                                .param("productId", PRODUCT.toString())
                                .param("branchtId", BRANCH.toString())
                )
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.data.quantityOnHand").isNumber());
    }

    @Test @Order(2)
    void testReserveStock() throws Exception {
        String body = """
                {
                  "productId": "%s",
                  "branchId": "%s",
                  "quantity": 3,
                  "reference": "TEST-RESERVATION"
                }
        """.formatted(PRODUCT, BRANCH);

        mvc.perform(authPost("/api/inventory/reserve", body))
                .andExpect(status().isOk());
    }

    @Test @Order(3)
    void testReleaseStock() throws Exception {
        String body = """
                {
                  "productId": "%s",
                  "branchId": "%s",
                  "quantity": 3,
                  "reference": "TEST-RELEASE"
                }
        """.formatted(PRODUCT, BRANCH);

        mvc.perform(authPost("/api/inventory/release", body))
                .andExpect(status().isOk());
    }

    @Test @Order(4)
    void testDecrementStock() throws Exception {
        String body = """
                {
                  "productId": "%s",
                  "branchId": "%s",
                  "quantity": 2,
                  "reference": "TEST-DECREMENT"
                }
        """.formatted(PRODUCT, BRANCH);

        mvc.perform(authPost("/api/inventory/decrement", body))
                .andExpect(status().isOk());
    }

    @Test @Order(5)
    void testLowStockReport() throws Exception {
        mvc.perform(authGet("/api/inventory/low-stock")
                        .param("threshold", "20"))
                .andExpect(status().isOk());
    }

    @Test @Order(6)
    void testInventoryValuation() throws Exception {
        mvc.perform(authGet("/api/inventory/valuation"))
                .andExpect(status().isOk());
    }
}