package com.IntegrityTechnologies.business_manager.common;

import java.security.SecureRandom;

public class TxnCodeGenerator {
    private static final String ALPHANUM = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    private static final SecureRandom rnd = new SecureRandom();

    public static String generate() {
        // generate 10 chars by default: e.g., "9F3K2W1AB7"
        StringBuilder sb = new StringBuilder(10);
        for (int i = 0; i < 10; i++) {
            sb.append(ALPHANUM.charAt(rnd.nextInt(ALPHANUM.length())));
        }
        return sb.toString();
    }
}