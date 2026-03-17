package com.IntegrityTechnologies.business_manager.config.files;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class ImagesUploadForm {
    private List<FIleUploadDTO> userImagesFiles = new ArrayList<>();
}
