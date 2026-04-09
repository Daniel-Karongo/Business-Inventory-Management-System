import { Injectable } from '@angular/core';

interface DeviceIdentity {
  id: string;
  createdAt: number;
}

@Injectable({ providedIn: 'root' })
export class DeviceService {

  private readonly STORAGE_KEY = 'device_identity_v1';

  private cached: DeviceIdentity | null = null;

  getDeviceId(): string {

    if (this.cached) {
      return this.cached.id;
    }

    const stored = localStorage.getItem(this.STORAGE_KEY);

    if (stored) {
      try {
        const parsed: DeviceIdentity = JSON.parse(stored);

        if (parsed?.id) {
          this.cached = parsed;
          return parsed.id;
        }
      } catch {
        // fallback to regenerate
      }
    }

    const identity: DeviceIdentity = {
      id: crypto.randomUUID(),
      createdAt: Date.now()
    };

    localStorage.setItem(this.STORAGE_KEY, JSON.stringify(identity));
    this.cached = identity;

    return identity.id;
  }

}