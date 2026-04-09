import { Injectable } from '@angular/core';

interface DeviceIdentity {
  id: string;
  token?: string;
  createdAt: number;
}

@Injectable({ providedIn: 'root' })
export class DeviceService {

  private readonly STORAGE_KEY = 'device_identity_v2';
  private cached: string | null = null;

  getDeviceId(): string {

    if (this.cached) return this.cached;

    const existing = localStorage.getItem(this.STORAGE_KEY);

    if (existing) {
      this.cached = existing;
      return existing;
    }

    const id = crypto.randomUUID();
    localStorage.setItem(this.STORAGE_KEY, id);
    this.cached = id;

    return id;
  }
}