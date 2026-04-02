package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import org.springframework.stereotype.Service;

@Service
public class LocationSecurityService {

    public void validate(Branch branch, Double lat, Double lng, Double accuracy) {

        if (!Boolean.TRUE.equals(branch.getEnforceGeofence())) return;

        if (lat == null || lng == null) {
            throw new SecurityException("Location required");
        }

        double distance = distanceMeters(
                branch.getLatitude(),
                branch.getLongitude(),
                lat,
                lng
        );

        if (distance > branch.getRadiusMeters()) {
            throw new SecurityException("Outside branch location");
        }

        if (accuracy != null && accuracy > branch.getRadiusMeters()) {
            throw new SecurityException("Location accuracy too low");
        }
    }

    private double distanceMeters(double lat1, double lon1, double lat2, double lon2) {

        double R = 6371000;

        double dLat = Math.toRadians(lat2 - lat1);
        double dLon = Math.toRadians(lon2 - lon1);

        double a =
                Math.sin(dLat / 2) * Math.sin(dLat / 2) +
                        Math.cos(Math.toRadians(lat1)) *
                                Math.cos(Math.toRadians(lat2)) *
                                Math.sin(dLon / 2) *
                                Math.sin(dLon / 2);

        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

        return R * c;
    }
}