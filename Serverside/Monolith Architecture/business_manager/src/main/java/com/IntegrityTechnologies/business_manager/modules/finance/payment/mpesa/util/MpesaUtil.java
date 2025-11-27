package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.util;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.config.MpesaProperties;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Base64;

public class MpesaUtil {

    public static String timestamp() {
        return LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
    }

    public static String generateStkPassword(MpesaProperties props, String timestamp) {
        String input = props.getShortcode() + props.getPasskey() + timestamp;
        return Base64.getEncoder().encodeToString(input.getBytes());
    }
}