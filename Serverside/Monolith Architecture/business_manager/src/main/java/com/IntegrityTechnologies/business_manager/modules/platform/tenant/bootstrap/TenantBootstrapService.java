package com.IntegrityTechnologies.business_manager.modules.platform.tenant.bootstrap;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed.BranchChartOfAccountsService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxSystemStateRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.branch.model.BranchAudit;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.DepartmentAudit;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.DepartmentMembershipRole;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserBranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserDepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantBootstrapService {

    private final BranchRepository branchRepository;
    private final DepartmentRepository departmentRepository;
    private final UserRepository userRepository;

    private final BranchAuditRepository branchAuditRepository;
    private final DepartmentAuditRepository departmentAuditRepository;
    private final UserAuditRepository userAuditRepository;

    private final UserDepartmentRepository userDepartmentRepository;
    private final UserBranchRepository userBranchRepository;

    private final BranchAccountingSettingsRepository accountingSettingsRepository;
    private final BranchChartOfAccountsService chartService;

    private final TaxSystemStateRepository taxSystemStateRepository;
    private final TaxProperties taxProperties;

    private final PasswordEncoder passwordEncoder;

    public boolean bootstrapTenant(UUID tenantId, String adminUsername, String adminPassword) {

        /* =====================================
           1️⃣ ENSURE MAIN BRANCH
        ===================================== */

        Branch branch = branchRepository
                .findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(tenantId, "MAIN")
                .orElseGet(() -> {

                    Branch b = Branch.builder()
                            .tenantId(tenantId)
                            .branchCode("MAIN")
                            .name("Main Branch")
                            .location("Default Location")
                            .phone("+254700000000")
                            .email("main@tenant.com")

                            // ✅ CRITICAL
                            .enforceGeofence(false)

                            .deleted(false)
                            .build();

                    branchRepository.save(b);

                    branchAuditRepository.save(
                            BranchAudit.builder()
                                    .branchId(b.getId())
                                    .branchName(b.getName())
                                    .tenantId(tenantId)
                                    .action("CREATE")
                                    .reason("Tenant bootstrap")
                                    .performedByUsername("system")
                                    .build()
                    );

                    return b;
                });

        /* =====================================
           2️⃣ ENSURE ACCOUNTING SETTINGS
        ===================================== */

        if (!accountingSettingsRepository.existsByTenantIdAndBranchId(tenantId, branch.getId())) {

            chartService.seedForBranch(
                    tenantId,
                    branch.getId()
            );

            accountingSettingsRepository.save(
                    BranchAccountingSettings.builder()
                            .branchId(branch.getId())
                            .revenueRecognitionMode(RevenueRecognitionMode.DELIVERY)
                            .build()
            );
        }

        /* =====================================
           2️⃣B ENSURE TAX CONFIGURATION
        ===================================== */

        if (taxSystemStateRepository.findByTenantIdAndBranchId(
                tenantId,
                branch.getId()
        ).isEmpty()) {

            taxSystemStateRepository.save(
                    TaxSystemState.builder()
                            .tenantId(tenantId)
                            .branchId(branch.getId())
                            .taxMode(taxProperties.getBusinessTaxMode())
                            .vatEnabled(taxProperties.isVatEnabled())
                            .pricesVatInclusive(taxProperties.isPricesVatInclusive())
                            .vatRate(taxProperties.getVatRate())
                            .corporateTaxRate(taxProperties.getCorporateTaxRate())
                            .locked(false)
                            .build()
            );
        }
        /* =====================================
           3️⃣ ENSURE GENERAL DEPARTMENT
        ===================================== */

        Department department = departmentRepository.findByTenantIdAndNameIgnoreCaseAndBranch_Id(
                        tenantId,
                        "GENERAL",
                        branch.getId()
                )
                .orElseGet(() -> {

                    Department d = Department.builder()
                            .tenantId(tenantId)
                            .branchId(branch.getId())
                            .branch(branch)
                            .name("GENERAL")
                            .description("Default general department")
                            .rollcallStartTime(LocalTime.of(9, 0))
                            .gracePeriodMinutes(15)
                            .deleted(false)
                            .build();

                    departmentRepository.save(d);

                    departmentAuditRepository.save(
                            DepartmentAudit.builder()
                                    .departmentId(d.getId())
                                    .departmentName(d.getName())
                                    .action("CREATE")
                                    .reason("Tenant bootstrap")
                                    .performedByUsername("system")
                                    .build()
                    );

                    return d;
                });

        /* =====================================
        4️⃣ ENSURE ADMIN USER
        ===================================== */

        boolean adminCreated = false;

        User admin = userRepository
                .findByUsernameAndTenantId(adminUsername, tenantId)
                .orElse(null);

        if (admin == null) {

            adminCreated = true;

            admin = User.builder()
                    .tenantId(tenantId)
                    .username(adminUsername)
                    .password(passwordEncoder.encode(adminPassword))
                    .role(Role.SUPERUSER)
                    .uploadFolder(UUID.randomUUID().toString())
                    .mustChangePassword(true)
                    .deleted(false)
                    .build();

            userRepository.save(admin);

            userAuditRepository.save(
                    UserAudit.builder()
                            .userId(admin.getId())
                            .username(admin.getUsername())
                            .action("CREATE")
                            .reason("Tenant bootstrap")
                            .performedByUsername("system")
                            .build()
            );
        }

        /* =====================================
           5️⃣ ENSURE USER ↔ BRANCH
        ===================================== */

        boolean branchAssigned = userBranchRepository
                .existsByUser_IdAndBranch_Id(
                        admin.getId(),
                        branch.getId()
                );

        if (!branchAssigned) {

            UserBranch userBranch = UserBranch.builder()
                    .user(admin)
                    .branch(branch)
                    .primaryBranch(true)
                    .build();

            userBranchRepository.save(userBranch);
        }


        /* =====================================
           6️⃣ ENSURE USER ↔ DEPARTMENT
        ===================================== */

        boolean deptAssigned = userDepartmentRepository
                .existsByUser_IdAndDepartment_Id(
                        admin.getId(),
                        department.getId()
                );

        if (!deptAssigned) {

            UserDepartment userDepartment = UserDepartment.builder()
                    .user(admin)
                    .department(department)
                    .role(DepartmentMembershipRole.HEAD)
                    .primaryDepartment(true)
                    .build();

            userDepartmentRepository.save(userDepartment);
        }

        return adminCreated;
    }
}