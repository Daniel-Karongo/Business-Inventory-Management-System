package com.IntegrityTechnologies.business_manager.modules.supplier.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SupplierCreateDTO {

    @NotBlank(message = "Supplier name is required.")
    private String name;

    @Schema(description = "List of supplier email addresses")
    @NotEmpty(message = "At least one email address is required.")
    private Set<@Email(message = "Invalid email address format.") String> email = Set.of();

    @Schema(description = "List of supplier phone numbers")
    @NotEmpty(message = "At least one phone number is required.")
    private Set<@NotBlank(message = "Phone number cannot be blank.") String> phoneNumber = Set.of();

    private String address;
    private String region;

    private Set<Long> categoryIds = Set.of();

    @Schema(type = "array")
    private Set<MultipartFile> images = Set.of();
}