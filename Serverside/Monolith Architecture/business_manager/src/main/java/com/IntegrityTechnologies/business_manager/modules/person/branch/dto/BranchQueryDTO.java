package com.IntegrityTechnologies.business_manager.modules.person.branch.dto;

import lombok.Data;

@Data
public class BranchQueryDTO {

    private String search;

    private Boolean deleted = false;

    private int page = 0;

    private int size = 20;

    private String sortBy = "createdAt";

    private String sortDirection = "desc";
}