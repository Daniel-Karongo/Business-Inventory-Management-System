package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.exception.OutOfStockException;
import com.IntegrityTechnologies.business_manager.modules.person.function.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.function.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.AdjustStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.InventoryResponse;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.ReceiveStockRequest;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.SupplierUnit;
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
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class InventoryService {

    private final InventoryItemRepository inventoryItemRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final ProductRepository productRepository;
    private final BranchRepository branchRepository;

    private static final int MAX_RETRIES = 5;

    /** --------------------------------------------
     *  RECEIVE STOCK (per branch)
     *  -------------------------------------------- */
    @Transactional
    public ApiResponse receiveStock(ReceiveStockRequest req) {

        var product = productRepository.findByIdAndDeletedFalse(req.getProductId())
                .orElseThrow(() -> new IllegalArgumentException("Product not found"));

        Branch branch = branchRepository.findById(req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Branch not found"));

        InventoryItem item = inventoryItemRepository.findByProductIdAndBranchId(
                        req.getProductId(), req.getBranchId())
                .orElse(InventoryItem.builder()
                        .product(product)
                        .branch(branch)
                        .quantityOnHand(0L)
                        .quantityReserved(0L)
                        .build()
                );

        if (req.getSuppliers() == null || req.getSuppliers().isEmpty()) {
            throw new IllegalArgumentException("At least one supplier must be provided");
        }

        Long quantityStocked = 0L;
        for (SupplierUnit supplierUnit: req.getSuppliers()) {
            quantityStocked += supplierUnit.getUnitsSupplied();

            stockTransactionRepository.save(StockTransaction.builder()
                    .productId(req.getProductId())
                    .branchId(req.getBranchId())
                    .type(StockTransaction.TransactionType.RECEIPT)
                    .quantityDelta(supplierUnit.getUnitsSupplied())
                    .reference(req.getReference())
                    .supplierId(supplierUnit.getSupplierId())
                    .note(req.getNote())
                    .timestamp(LocalDateTime.now())
                    .performedBy(getCurrentUsername())
                    .build()
            );
        }

        item.setQuantityOnHand(item.getQuantityOnHand() + quantityStocked);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        return new ApiResponse("success", "Stock received", buildResponse(item));
    }

    /** --------------------------------------------
     *  ADJUST STOCK (per branch)
     *  -------------------------------------------- */
    @Transactional
    public ApiResponse adjustStock(AdjustStockRequest req) {

        var product = productRepository.findById(req.getProductId())
                .orElseThrow(() -> new IllegalArgumentException("Product not found"));

        Branch branch = branchRepository.findById(req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Branch not found"));

        InventoryItem item = inventoryItemRepository.findByProductIdAndBranchId(
                        req.getProductId(), req.getBranchId())
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        item.setQuantityOnHand(Math.max(0, item.getQuantityOnHand() + req.getQuantityDelta()));
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(req.getProductId())
                .branchId(req.getBranchId())
                .type(StockTransaction.TransactionType.ADJUSTMENT)
                .quantityDelta(req.getQuantityDelta())
                .note(req.getReason())
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );

        return new ApiResponse("success", "Stock updated", buildResponse(item));
    }

    /** --------------------------------------------
     *  GET INVENTORY PER PRODUCT & BRANCH
     *  -------------------------------------------- */

    public List<InventoryResponse> getAllInventory() {
        return inventoryItemRepository.findAll()
                .stream()
                .map(this::buildResponse)
                .toList();
    }

    public List<InventoryResponse> getInventoryByBranch(UUID branchId) {
        return inventoryItemRepository
                    .findByBranchId(branchId)
                    .stream()
                    .map(this::buildResponse)
                    .toList();
    }

    public ApiResponse getInventoryForBranchProduct(UUID productId, UUID branchId) {
        Object res ;
        if(branchId != null) {
            res = inventoryItemRepository
                    .findByProductIdAndBranchId(productId, branchId)
                    .map(this::buildResponse)
                    .orElse(null);
        } else {
            res = inventoryItemRepository
                    .findByProductId(productId)
                    .stream()
                    .map(item -> buildResponse(item))
                    .toList();
        }

        return new ApiResponse("success", "Inventory result", res);
    }

    /** --------------------------------------------
     *  DECREMENT STOCK (branch-aware + optimistic lock)
     *  -------------------------------------------- */
    @Transactional
    public void decrementStock(UUID productId, UUID branchId, int quantity, String reference) {
        if (quantity <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        int attempts = 0;

        while (true) {
            try {
                processStockDecrement(productId, branchId, quantity, reference);
                return;
            } catch (OptimisticLockException e) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Concurrent stock update conflict. Try again.");
                }
            }
        }
    }

    private void processStockDecrement(UUID productId, UUID branchId, long qty, String reference) {

        InventoryItem item = inventoryItemRepository.findByProductIdAndBranchId(productId, branchId)
                .orElseThrow(() -> new OutOfStockException("Inventory record not found"));

        long available = item.getQuantityOnHand() - item.getQuantityReserved();
        if (available < qty) {
            throw new OutOfStockException("Insufficient stock in this branch");
        }

        item.setQuantityOnHand(item.getQuantityOnHand() - qty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(productId)
                .branchId(branchId)
                .type(StockTransaction.TransactionType.SALE)
                .quantityDelta(-qty)
                .reference(reference)
                .note("Sale decrement")
                .timestamp(LocalDateTime.now())
                .performedBy(getCurrentUsername())
                .build()
        );
    }

    @Transactional
    public void reserveStock(UUID productId, UUID branchId, int quantity, String reference) {
        if (quantity <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        int attempts = 0;
        while (true) {
            try {
                processReservation(productId, branchId, quantity, reference);
                return;
            } catch (OptimisticLockException e) {
                attempts++;
                if (attempts >= MAX_RETRIES) {
                    throw new RuntimeException("Failed to reserve stock due to concurrent updates.");
                }
            }
        }
    }

    private void processReservation(UUID productId, UUID branchId, long quantity, String reference) {
        InventoryItem item = inventoryItemRepository.findByProductIdAndBranchId(productId, branchId)
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
                .branchId(branchId)
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
    public void releaseReservation(UUID productId, UUID branchId, int quantity, String reference) {
        if (quantity <= 0) throw new IllegalArgumentException("Quantity must be > 0");

        InventoryItem item = inventoryItemRepository.findByProductIdAndBranchId(productId, branchId)
                .orElseThrow(() -> new IllegalArgumentException("Inventory item not found"));

        long releaseQty = Math.min(quantity, item.getQuantityReserved());

        if(((item.getQuantityReserved() - releaseQty) < 0) || (item.getQuantityReserved() == 0)) {
            throw new IllegalArgumentException("You cannot release more than you have reserved, which is " + item.getQuantityReserved());
        }

        item.setQuantityReserved(item.getQuantityReserved() - releaseQty);
        item.setLastUpdatedAt(LocalDateTime.now());
        item.setLastUpdatedBy(getCurrentUsername());
        inventoryItemRepository.save(item);

        stockTransactionRepository.save(StockTransaction.builder()
                .productId(productId)
                        .branchId(branchId)
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
                .productName(item.getProduct().getName())
                .branchId(item.getBranch().getId())
                .branchName(item.getBranch().getName())
                .quantityOnHand(item.getQuantityOnHand())
                .quantityReserved(item.getQuantityReserved())
                .lastUpdatedAt(item.getLastUpdatedAt() != null ? item.getLastUpdatedAt().toString() : null)
                .build();
    }

    private String getCurrentUsername() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }
}