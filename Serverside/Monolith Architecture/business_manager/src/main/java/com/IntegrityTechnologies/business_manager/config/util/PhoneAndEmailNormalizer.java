package com.IntegrityTechnologies.business_manager.common;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

public class PhoneAndEmailNormalizer {
    public static Set<String> normalizeEmails(List<String> emails) {
        return emails != null ? emails.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .map(String::toLowerCase)
                .filter(s -> !s.isBlank())
                .collect(Collectors.toCollection(LinkedHashSet::new)) : new LinkedHashSet<>();
    }

    public static Set<String> normalizePhones(List<String> phones) {
        return phones != null ? phones.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .map(phone -> normalizePhone(phone))
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(LinkedHashSet::new)) : new LinkedHashSet<>();
    }

    public static String normalizePhone(String phone) {
        if (phone == null) return null;

        // 1️⃣ Remove spaces and hyphens
        String cleaned = phone.replaceAll("[\\s-]", "");

        // 2️⃣ Convert local formats to international
        if (cleaned.matches("^07\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        } else if (cleaned.matches("^01\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        }

        return cleaned;
    }

    public static String toTitleCase(String input) {
        if (input == null || input.isBlank()) return input;

        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;

        for (char c : input.toLowerCase().toCharArray()) {
            if (Character.isWhitespace(c)) {
                capitalizeNext = true;
                result.append(c);
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(c));
                capitalizeNext = false;
            } else {
                result.append(c);
            }
        }

        return result.toString();
    }
}
