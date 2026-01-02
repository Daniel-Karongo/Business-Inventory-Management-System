package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.RollcallMethod;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.RollcallStatus;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.Rollcall;
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
    List<Rollcall> findByDepartmentIdAndTimestampBetweenOrderByTimestampDesc(UUID departmentId, LocalDateTime from, LocalDateTime to);
    List<Rollcall> findByDepartmentIdAndTimestampBetweenAndStatusOrderByTimestampDesc(UUID departmentId, LocalDateTime from, LocalDateTime to, RollcallStatus status);
    List<Rollcall> findByBranchIdAndTimestampBetweenOrderByTimestampDesc(UUID branchId, LocalDateTime from, LocalDateTime to);
    List<Rollcall> findByBranchIdAndTimestampBetweenAndStatusOrderByTimestampDesc(UUID branchId, LocalDateTime from, LocalDateTime to, RollcallStatus status);

    Optional<Rollcall> findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethodNot(
            UUID userId,
            UUID departmentId,
            UUID branchId,
            LocalDate rollcallDate,
            RollcallMethod excludedMethod
    );

    Optional<Rollcall> findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethod(
            UUID userId,
            UUID departmentId,
            UUID branchId,
            LocalDate rollcallDate,
            RollcallMethod method
    );

    boolean existsByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndMethod(
            UUID userId,
            UUID departmentId,
            UUID branchId,
            LocalDate rollcallDate,
            RollcallMethod method
    );

    boolean existsByUserIdAndDepartmentIdAndBranchIdAndRollcallDate(UUID id, UUID id1, UUID id2, LocalDate today);

    Optional<Rollcall> findByUserIdAndDepartmentIdAndBranchIdAndRollcallDateAndStatus(
            UUID userId,
            UUID departmentId,
            UUID branchId,
            LocalDate rollcallDate,
            RollcallStatus status
    );
}