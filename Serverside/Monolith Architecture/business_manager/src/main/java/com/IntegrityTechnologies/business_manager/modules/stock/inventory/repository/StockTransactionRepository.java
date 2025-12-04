package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.StockTransaction;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface StockTransactionRepository extends JpaRepository<StockTransaction, UUID> {
    List<StockTransaction> findByProductIdOrderByTimestampDesc(UUID productId);
    List<StockTransaction> findByProductId(UUID productId);

    // receipts only for product (legacy)
    @Query("SELECT t FROM StockTransaction t WHERE t.productId = :pid AND t.branchId = :bid AND t.type = 'RECEIPT' ORDER BY t.timestamp ASC")
    List<StockTransaction> findReceipts(UUID pid, UUID bid);

    // variant-aware receipts (new)
    @Query("SELECT t FROM StockTransaction t WHERE t.productVariantId = :vid AND t.branchId = :bid AND t.type = 'RECEIPT' ORDER BY t.timestamp ASC")
    List<StockTransaction> findReceiptsByVariant(UUID vid, UUID bid);

    // find transactions between two datetimes (inclusive) for product (legacy)
    @Query("SELECT t FROM StockTransaction t WHERE t.productId = :pid AND t.branchId = :bid AND t.timestamp >= :from AND t.timestamp <= :to ORDER BY t.timestamp ASC")
    List<StockTransaction> findBetween(UUID pid, UUID bid, LocalDateTime from, LocalDateTime to);

    // variant-aware between
    @Query("SELECT s FROM StockTransaction s " +
            "WHERE s.productVariantId = :variantId " +
            "AND s.branchId = :branchId " +
            "AND s.timestamp BETWEEN :from AND :to " +
            "ORDER BY s.timestamp ASC")
    List<StockTransaction> findBetweenVariant(
            UUID variantId,
            UUID branchId,
            LocalDateTime from,
            LocalDateTime to
    );
}