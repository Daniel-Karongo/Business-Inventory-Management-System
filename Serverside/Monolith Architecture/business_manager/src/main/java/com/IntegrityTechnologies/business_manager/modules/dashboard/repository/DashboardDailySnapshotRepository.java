package com.IntegrityTechnologies.business_manager.modules.dashboard.repository;

import com.IntegrityTechnologies.business_manager.modules.dashboard.model.DashboardDailySnapshot;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface DashboardDailySnapshotRepository
        extends JpaRepository<DashboardDailySnapshot, UUID> {

    Optional<DashboardDailySnapshot> findByDate(LocalDate date);

    List<DashboardDailySnapshot> findTop30ByOrderByDateDesc();
}