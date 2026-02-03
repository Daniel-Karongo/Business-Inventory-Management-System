package com.IntegrityTechnologies.business_manager.modules.person.entity.system;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalTime;
import java.util.HashSet;

@Configuration
@RequiredArgsConstructor
@Slf4j
public class SystemInitializer {

    private final BranchRepository branchRepository;
    private final DepartmentRepository departmentRepository;

    @Bean
    @Transactional
    public ApplicationRunner initializeDefaults() {
        return args -> {

            Branch mainBranch = branchRepository.findByBranchCode("MAIN")
                    .orElseGet(() -> {
                        log.warn("⚠️ No branches found. Creating default MAIN branch.");
                        Branch b = Branch.builder()
                                .branchCode("MAIN")
                                .name("Main Branch")
                                .location("Default Location")
                                .phone("+254700000000")
                                .email("main@default.com")
                                .build();
                        branchRepository.save(b);
                        log.info("✅ Default MAIN branch created.");
                        return b;
                    });

            Department generalDept = departmentRepository.findByNameIgnoreCase("GENERAL")
                    .orElseGet(() -> {
                        log.warn("⚠️ No departments found. Creating default GENERAL department.");
                        Department d = Department.builder()
                                .name("GENERAL")
                                .description("Default general department")
                                .rollcallStartTime(LocalTime.of(9, 0))
                                .gracePeriodMinutes(15)
                                .build();
                        departmentRepository.save(d);
                        log.info("✅ Default GENERAL department created.");
                        return d;
                    });

            boolean linked = branchRepository.branchContainsDepartment(mainBranch.getId(), generalDept.getId());

            if (!linked) {
                log.info("Linking GENERAL department to MAIN branch...");
                mainBranch.getDepartments().add(generalDept);
                branchRepository.save(mainBranch);
                log.info("✅ GENERAL department linked to MAIN branch.");
            }

            log.info("🎉 System initialization complete.");
        };
    }
}