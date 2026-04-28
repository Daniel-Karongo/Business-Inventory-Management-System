package com.IntegrityTechnologies.business_manager.modules.person.user.model;

public enum Role {

    SUPERUSER(4),
    ADMIN(3),
    MANAGER(2),
    SUPERVISOR(1),
    EMPLOYEE(0);

    private final int level;

    Role(int level){
        this.level = level;
    }

    public int getLevel(){
        return level;
    }

    // Keep for endpoint inheritance
    public boolean canAccess(Role other){
        return this.level >= other.level;
    }

    // NEW: strict management only
    public boolean canManage(Role other){
        return this.level > other.level;
    }
}