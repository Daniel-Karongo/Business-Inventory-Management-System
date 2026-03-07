package com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserImage;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserImageRepository extends JpaRepository<UserImage, UUID> {

    List<UserImage> findByUser(User user);

    Optional<UserImage> findByUserAndFileName(User user, String filename);

    Page<UserImage> findByDeleted(Boolean deleted, Pageable pageable);

}