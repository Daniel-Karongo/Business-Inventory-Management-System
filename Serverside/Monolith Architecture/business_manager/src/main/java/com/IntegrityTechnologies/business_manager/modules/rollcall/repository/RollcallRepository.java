package com.IntegrityTechnologies.business_manager.modules.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.rollcall.model.RollcallStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface RollcallRepository extends JpaRepository<Rollcall, UUID> {
    List<Rollcall> findByUserIdAndTimestampBetween(UUID userId, LocalDateTime from, LocalDateTime to);
    List<Rollcall> findByDepartmentIdAndTimestampBetween(UUID departmentId, LocalDateTime from, LocalDateTime to);
    List<Rollcall> findByDepartmentIdAndTimestampBetweenAndStatus(UUID departmentId, LocalDateTime from, LocalDateTime to, RollcallStatus status);
}
