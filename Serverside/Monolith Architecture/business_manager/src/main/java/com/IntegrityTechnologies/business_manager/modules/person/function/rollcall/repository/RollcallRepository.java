package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.RollcallStatus;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.Rollcall;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface RollcallRepository extends JpaRepository<Rollcall, UUID> {
    List<Rollcall> findByUserIdAndTimestampBetweenOrderByTimestampDesc(UUID userId, LocalDateTime from, LocalDateTime to);
    List<Rollcall> findByDepartmentIdAndTimestampBetweenOrderByTimestampDesc(UUID departmentId, LocalDateTime from, LocalDateTime to);
    List<Rollcall> findByDepartmentIdAndTimestampBetweenAndStatusOrderByTimestampDesc(UUID departmentId, LocalDateTime from, LocalDateTime to, RollcallStatus status);
}
