package com.IntegrityTechnologies.business_manager.modules.dashboard.repository;

import com.IntegrityTechnologies.business_manager.modules.dashboard.model.DashboardDailySnapshot;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface DashboardDailySnapshotRepository
        extends JpaRepository<DashboardDailySnapshot, UUID> {

    Optional<DashboardDailySnapshot> findByDate(LocalDate date);

    @Query("""
        SELECT COALESCE(SUM(s.cogs), 0)
        FROM DashboardDailySnapshot s
        WHERE s.date BETWEEN :from AND :to
    """)
    BigDecimal sumCogsBetween(
            @Param("from") LocalDate from,
            @Param("to") LocalDate to
    );

    @Query("""
        SELECT s.date
        FROM DashboardDailySnapshot s
        WHERE s.date BETWEEN :from AND :to
    """)
    List<LocalDate> findExistingDatesBetween(
            @Param("from") LocalDate from,
            @Param("to") LocalDate to
    );
}