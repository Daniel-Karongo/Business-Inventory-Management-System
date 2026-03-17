package com.IntegrityTechnologies.business_manager.common;

import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

@Data
public class FIleUploadDTO {
    private MultipartFile file;
    private String description;
}