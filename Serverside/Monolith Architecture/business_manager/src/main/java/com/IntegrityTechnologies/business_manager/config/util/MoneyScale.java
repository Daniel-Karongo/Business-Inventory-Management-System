package com.IntegrityTechnologies.business_manager.config.util;

import java.math.RoundingMode;

public final class MoneyScale {
    public static final int INTERNAL_SCALE = 6;
    public static final int MONEY_SCALE = 2;

    public static final RoundingMode ROUNDING =
            RoundingMode.HALF_UP;

    private MoneyScale() {}
}