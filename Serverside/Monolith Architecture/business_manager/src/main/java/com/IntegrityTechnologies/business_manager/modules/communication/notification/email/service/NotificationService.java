package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;

public interface NotificationService {
//   Notify department heads that a user is absent for a rollcall
    void notifyAbsent(Department dept, User absentUser, Rollcall absentRollcall);

//    Other notifications (optional): notifyLate, notifyRolledIn, etc.
    default void notifyLate(Department dept, User lateUser, Rollcall rollcall) {}
}