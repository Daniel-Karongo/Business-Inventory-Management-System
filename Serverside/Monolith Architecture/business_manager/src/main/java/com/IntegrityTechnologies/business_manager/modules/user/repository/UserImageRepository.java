package com.IntegrityTechnologies.business_manager.modules.user.repository;

import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.model.UserImage;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserImageRepository extends JpaRepository<UserImage, UUID> {

    List<UserImage> findByUser(User user);

    List<UserImage> findByUserAndDeletedFalse(User user);

    List<UserImage> findByUserId(UUID userId);

    Optional<UserImage> findByUserAndFileName(User user, String fileName);
}