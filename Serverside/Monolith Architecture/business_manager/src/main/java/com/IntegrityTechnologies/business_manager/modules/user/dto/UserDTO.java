package com.IntegrityTechnologies.business_manager.modules.user.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserDTO {

    private UUID id;

    @NotBlank(message = "Username is required")
    private String username;

    private String password;

    private String emailAddress;

    private String idNumber;

    private String role;

    private String createdBy;

    private boolean deleted;

    @Schema(
            description = "List of ID images (upload only)",
            type = "array"
//            format = "binary"
    )
    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private List<MultipartFile> idImageFiles;


    @Schema(description = "List of stored ID image URLs (response only)")
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private List<String> idImageUrls;
}