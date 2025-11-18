package com.IntegrityTechnologies.business_manager.modules.user.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserDTO {

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private UUID id;

    @NotBlank
    private String username;
    private String password;
    private List<@Email String> emailAddresses;
    private List<String> phoneNumbers;
    private String idNumber;
    @NotBlank
    private String role;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private String createdBy;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private String lastModifiedBy;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private LocalDateTime createdAt;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private LocalDateTime lastModifiedAt;

    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private boolean deleted;

    @Schema(description = "List of ID images (upload only)", type = "array")
    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private List<MultipartFile> idImageFiles;

    @Schema(description = "List of stored ID image URLs (response only)")
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    private List<String> idImageUrls;
}