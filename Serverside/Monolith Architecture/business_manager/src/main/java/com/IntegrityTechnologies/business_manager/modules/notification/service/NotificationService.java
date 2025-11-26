package com.IntegrityTechnologies.business_manager.modules.notification.service;

import com.IntegrityTechnologies.business_manager.modules.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;

public interface NotificationService {
//   Notify department heads that a user is absent for a rollcall
    void notifyAbsent(Department dept, User absentUser, Rollcall absentRollcall);

//    Other notifications (optional): notifyLate, notifyRolledIn, etc.
    default void notifyLate(Department dept, User lateUser, Rollcall rollcall) {}
}