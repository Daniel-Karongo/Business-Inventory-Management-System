package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallMethod;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.RollcallStatus;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.Rollcall;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface RollcallRepository extends JpaRepository<Rollcall, UUID> {
    List<Rollcall> findByUserIdAndTimestampBetweenOrderByTimestampDesc(UUID userId, LocalDateTime from, LocalDateTime to);
    List<Rollcall> findByBranchIdAndTimestampBetweenOrderByTimestampDesc(UUID branchId, LocalDateTime from, LocalDateTime to);

    Optional<Rollcall> findByUserIdAndBranchIdAndRollcallDateAndMethodNot(
            UUID userId,
            UUID branchId,
            LocalDate rollcallDate,
            RollcallMethod excludedMethod
    );

    Optional<Rollcall> findByUserIdAndBranchIdAndRollcallDateAndMethod(
            UUID userId,
            UUID branchId,
            LocalDate rollcallDate,
            RollcallMethod method
    );

    Optional<Rollcall> findByUserIdAndBranchIdAndRollcallDateAndStatus(
            UUID userId,
            UUID branchId,
            LocalDate rollcallDate,
            RollcallStatus status
    );
}