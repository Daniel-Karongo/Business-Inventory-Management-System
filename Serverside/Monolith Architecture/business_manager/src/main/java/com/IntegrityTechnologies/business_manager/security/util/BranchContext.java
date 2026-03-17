package com.IntegrityTechnologies.business_manager.security;

import java.util.UUID;

public final class BranchContext {

    private static final InheritableThreadLocal<UUID> CONTEXT =
            new InheritableThreadLocal<>();

    private BranchContext() {}

    public static void set(UUID branchId) {
        CONTEXT.set(branchId);
    }

    public static UUID get() {

        UUID branchId = CONTEXT.get();

        if (branchId == null) {
            return null;
        }

        return branchId;
    }

    public static void clear() {
        CONTEXT.remove();
    }

    public static UUID getOrNull() {
        return CONTEXT.get();
    }
}