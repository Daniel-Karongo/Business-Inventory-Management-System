import { Injectable } from '@angular/core';

export interface DeviceLocation {
  latitude: number;
  longitude: number;
  accuracy: number;
}

@Injectable({ providedIn: 'root' })
export class LocationService {

  async getLocation(): Promise<DeviceLocation> {

    if (!navigator.geolocation) {

      return {
        latitude: 0,
        longitude: 0,
        accuracy: 0
      };

    }

    return new Promise<DeviceLocation>((resolve) => {

      let resolved = false;

      const fallbackTimer = setTimeout(() => {

        if (resolved) {
          return;
        }

        resolved = true;

        resolve({
          latitude: 0,
          longitude: 0,
          accuracy: 0
        });

      }, 8000);

      navigator.geolocation.getCurrentPosition(

        position => {

          if (resolved) {
            return;
          }

          resolved = true;

          clearTimeout(fallbackTimer);

          resolve({
            latitude: position.coords.latitude,
            longitude: position.coords.longitude,
            accuracy: position.coords.accuracy
          });

        },

        () => {

          if (resolved) {
            return;
          }

          resolved = true;

          clearTimeout(fallbackTimer);

          resolve({
            latitude: 0,
            longitude: 0,
            accuracy: 0
          });

        },

        {
          enableHighAccuracy: false,
          timeout: 7000,
          maximumAge: 300000
        }
      );

    });
  }
}