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

    const raw = localStorage.getItem(this.STORAGE_KEY);

    if (raw) {
      try {
        const parsed = JSON.parse(raw);

        // ✅ handle old object format
        if (parsed?.id) {
          this.cached = parsed.id;
          localStorage.setItem(this.STORAGE_KEY, parsed.id);
          if(this.cached !== null )
            return this.cached;
        }

        this.cached = raw;
        return raw;

      } catch {
        this.cached = raw;
        return raw;
      }
    }

    const id = crypto.randomUUID();
    localStorage.setItem(this.STORAGE_KEY, id);
    this.cached = id;

    return id;
  }
}