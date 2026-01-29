package com.IntegrityTechnologies.business_manager.security.acl.util;

import java.util.regex.Pattern;

public final class PathTemplateNormalizer {

    private static final Pattern UUID =
            Pattern.compile("[0-9a-fA-F\\-]{36}");

    private static final Pattern NUMBER =
            Pattern.compile("\\d+");

    private PathTemplateNormalizer() {}

    public static String normalize(String path) {

        String p = path;

        p = UUID.matcher(p).replaceAll("{id}");
        p = NUMBER.matcher(p).replaceAll("{id}");

        return p;
    }
}