import { Injectable } from '@angular/core';

export interface DeviceLocation {
  latitude: number;
  longitude: number;
  accuracy: number;
}

@Injectable({
  providedIn: 'root'
})
export class LocationService {

  async getLocation(): Promise<DeviceLocation> {

    if (!navigator.geolocation) {

      console.error(
        '[LocationService] Geolocation API not supported by browser'
      );

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

        console.error(
          '[LocationService] Geolocation timeout fallback triggered'
        );

        resolve({
          latitude: 0,
          longitude: 0,
          accuracy: 0
        });

      }, 15000);

      navigator.geolocation.getCurrentPosition(

        (position) => {

          if (resolved) {
            return;
          }

          resolved = true;

          clearTimeout(fallbackTimer);

          console.info(
            '[LocationService] Geolocation success',
            {
              latitude: position.coords.latitude,
              longitude: position.coords.longitude,
              accuracy: position.coords.accuracy
            }
          );

          resolve({
            latitude: position.coords.latitude,
            longitude: position.coords.longitude,
            accuracy: position.coords.accuracy
          });
        },

        (error) => {

          if (resolved) {
            return;
          }

          resolved = true;

          clearTimeout(fallbackTimer);

          console.error(
            '[LocationService] Geolocation failure',
            {
              code: error.code,
              message: error.message
            }
          );

          resolve({
            latitude: 0,
            longitude: 0,
            accuracy: 0
          });
        },

        {
          enableHighAccuracy: true,
          timeout: 15000,
          maximumAge: 0
        }
      );
    });
  }
}