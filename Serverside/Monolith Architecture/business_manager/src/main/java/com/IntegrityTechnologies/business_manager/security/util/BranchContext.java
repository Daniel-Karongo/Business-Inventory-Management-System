package com.IntegrityTechnologies.business_manager.security.util;

import java.util.UUID;

public final class BranchContext {

    private static final ThreadLocal<UUID> CONTEXT = new ThreadLocal<>();

    private BranchContext() {}

    public static void set(UUID branchId) {
        CONTEXT.set(branchId);
    }

    public static UUID get() {
        return CONTEXT.get();
    }

    public static UUID getOrNull() {
        return CONTEXT.get();
    }

    public static void clear() {
        CONTEXT.remove();
    }
}