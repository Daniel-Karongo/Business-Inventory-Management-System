package com.IntegrityTechnologies.business_manager.security.util;

import java.util.UUID;
import java.util.concurrent.Executor;

public class TenantAwareExecutor implements Executor {

    private final Executor delegate;

    public TenantAwareExecutor(Executor delegate) {
        this.delegate = delegate;
    }

    @Override
    public void execute(Runnable command) {

        UUID tenantId = TenantContext.getOrNull();
        UUID branchId = BranchContext.getOrNull();

        delegate.execute(() -> {
            try {
                if (tenantId != null) {
                    TenantContext.setTenantId(tenantId);
                }

                if (branchId != null) {
                    BranchContext.set(branchId);
                }

                command.run();

            } finally {
                TenantContext.clear();
                BranchContext.clear();
            }
        });
    }
}