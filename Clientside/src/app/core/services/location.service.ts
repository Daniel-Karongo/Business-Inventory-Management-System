import { Injectable } from '@angular/core';

export interface DeviceLocation {
  latitude: number;
  longitude: number;
  accuracy: number;
}

@Injectable({ providedIn: 'root' })
export class LocationService {

  getLocation(): Promise<DeviceLocation> {

    if (!('geolocation' in navigator)) {
      return Promise.reject(new Error('Location required'));
    }

    return new Promise((resolve, reject) => {

      navigator.geolocation.getCurrentPosition(

        position => {
          resolve({
            latitude: position.coords.latitude,
            longitude: position.coords.longitude,
            accuracy: position.coords.accuracy
          });
        },

        () => {
          reject(new Error('Location required'));
        },

        {
          enableHighAccuracy: true,
          timeout: 10000,
          maximumAge: 0
        }
      );

    });
  }

}