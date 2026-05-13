package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.dto;

import lombok.Data;

@Data
public class BranchMpesaSettingsDTO {

    private Boolean enabled;

    private Boolean sandbox;

    private String shortcode;

    private String consumerKey;

    private String consumerSecret;

    private String passkey;

    private String securityCredential;

    private String stkCallbackUrl;

    private String c2bValidationUrl;

    private String c2bConfirmationUrl;

    private String initiatorName;

    private Boolean active;
}