package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.exception.OutOfStockException;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.AdjustStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.InventoryResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ReceiveStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryItem;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.StockTransaction;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.repository.ProductRepository;

import jakarta.persistence.OptimisticLockException;

import lombok.RequiredArgsConstructor;

import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class InventoryService {

    private final InventoryItemRepository inventoryItemRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final ProductRepository productRepository;

    private static final int MAX_RETRIES = 5;

    /** --------------------------------------------
     *  CREATE / RECEIVE STOCK
     *  -------------------------------------------- */
    @Transactional
    public InventoryResponse receiveStock(ReceiveStockRequest req) {
        var product = productRepository.findById(req.getProductId())
                .orElseThrow(() -> new IllegalArgumentException("Product not found"));

        InventoryItem item = inventoryItemRepository.findByProductId(req.getProductId())
                .orElse(InventoryItem.builder()
                        .product(product)
                        .quantityOnHand(0L)
                        .quantityReserved(0L)
                        .location(req.getLocation() != null ? req.getLocation() : "MAIN")
                        .build());

        item.setQuantityOnHand(item.getQuantityOnHand() + req.getQuantity());
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(req.getProductId())
                .type(StockTransaction.TransactionType.RECEIPT)
                .quantityDelta(req.getQuantity())
                .reference(req.getReference())
                .note(req.getNote())
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );

        return buildResponse(item);
    }

    /** --------------------------------------------
     *  ADJUST STOCK
     *  -------------------------------------------- */
    @Transactional
    public InventoryResponse adjustStock(AdjustStockRequest req) {
        var product = productRepository.findById(req.getProductId())
                .orElseThrow(() -> new IllegalArgumentException("Product not found"));

        InventoryItem item = inventoryItemRepository.findByProductId(req.getProductId())
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        long newQty = item.getQuantityOnHand() + req.getQuantityDelta();
        item.setQuantityOnHand(Math.max(0, newQty));
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(req.getProductId())
                .type(StockTransaction.TransactionType.ADJUSTMENT)
                .quantityDelta(req.getQuantityDelta())
                .note(req.getReason())
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );

        return buildResponse(item);
    }

    /** --------------------------------------------
     *  GET INVENTORY
     *  -------------------------------------------- */
    public InventoryResponse getInventoryForProduct(UUID productId) {
        return inventoryItemRepository.findByProductId(productId)
                .map(this::buildResponse)
                .orElse(null);
    }

    /** --------------------------------------------
     *  NEW — Optimistic-Lock Safe Stock Decrement
     *  -------------------------------------------- */
    @Transactional
    public void decrementStock(UUID productId, int quantity, String reference) {
        if (quantity <= 0) {
            throw new IllegalArgumentException("Quantity must be > 0");
        }

        int attempts = 0;
        while (true) {
            try {
                processStockDecrement(productId, quantity, reference);
                return; // success
            } catch (OptimisticLockException e) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Failed to decrement stock due to concurrent updates. Please retry.");
                }
            }
        }
    }

    private void processStockDecrement(UUID productId, long quantity, String reference) {
        productRepository.findById(productId)
                .orElseThrow(() -> new IllegalArgumentException("Product not found"));

        InventoryItem item = inventoryItemRepository.findByProductId(productId)
                .orElseThrow(() -> new OutOfStockException("No inventory record found"));

        long available = item.getQuantityOnHand() - item.getQuantityReserved();
        if (available < quantity) {
            throw new OutOfStockException("Insufficient stock. Available: " + available + ", required: " + quantity);
        }

        item.setQuantityOnHand(item.getQuantityOnHand() - quantity);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());

        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(productId)
                .type(StockTransaction.TransactionType.SALE)
                .quantityDelta(-Math.abs(quantity))
                .reference(reference)
                .note("Sale consumption")
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );
    }

    /** --------------------------------------------
     *  OPTIONAL: RESERVATION / RELEASE
     *  -------------------------------------------- */

    @Transactional
    public void reserveStock(UUID productId, int quantity, String reference) {
        if (quantity <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        int attempts = 0;
        while (true) {
            try {
                processReservation(productId, quantity, reference);
                return;
            } catch (OptimisticLockException e) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Failed to reserve stock due to concurrent updates.");
                }
            }
        }
    }

    private void processReservation(UUID productId, long quantity, String reference) {
        InventoryItem item = inventoryItemRepository.findByProductId(productId)
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        long available = item.getQuantityOnHand() - item.getQuantityReserved();
        if (available < quantity) {
            throw new OutOfStockException("Not enough stock to reserve");
        }

        item.setQuantityReserved(item.getQuantityReserved() + quantity);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(productId)
                .type(StockTransaction.TransactionType.RESERVATION)
                .quantityDelta(quantity)
                .reference(reference)
                .note("Reserved for order")
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );
    }

    @Transactional
    public void releaseReservation(UUID productId, int quantity, String reference) {
        if (quantity <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        InventoryItem item = inventoryItemRepository.findByProductId(productId)
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        long releaseQty = Math.min(quantity, item.getQuantityReserved());

        item.setQuantityReserved(item.getQuantityReserved() - releaseQty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(productId)
                .type(StockTransaction.TransactionType.RELEASE)
                .quantityDelta(-releaseQty)
                .reference(reference)
                .note("Release reservation")
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );
    }

    /** --------------------------------------------
     *  Helpers
     *  -------------------------------------------- */

    private InventoryResponse buildResponse(InventoryItem item) {
        return InventoryResponse.builder()
                .productId(item.getProduct().getId())
                .quantityOnHand(item.getQuantityOnHand())
                .quantityReserved(item.getQuantityReserved())
                .location(item.getLocation())
                .lastUpdatedAt(item.getLastUpdatedAt() != null ? item.getLastUpdatedAt().toString() : null)
                .build();
    }

    private String getCurrentUsername() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return (auth != null && auth.getName() != null) ? auth.getName() : "SYSTEM";
    }
}