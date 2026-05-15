package com.IntegrityTechnologies.business_manager.config.util;

import lombok.experimental.UtilityClass;

@UtilityClass
public class SecretMaskingUtil {

    public String mask(String value) {

        if (value == null || value.isBlank()) {
            return "";
        }

        return "********";
    }
}