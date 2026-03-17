package com.IntegrityTechnologies.business_manager.config.files;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class FIleUploadDTO {
    private MultipartFile file;
    private String description;
}