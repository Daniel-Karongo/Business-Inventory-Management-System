import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class ThemeService {

  private key = 'theme-preference';

  constructor() {
    this.applyStoredPreference();

    // Listen for OS theme changes
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => {
      const saved = localStorage.getItem(this.key) as 'light' | 'dark' | 'system';
      if (saved === 'system') this.applyPreference('system');
    });
  }

  setPreference(pref: 'light' | 'dark' | 'system') {
    localStorage.setItem(this.key, pref);
    this.applyPreference(pref);
  }

  private applyStoredPreference() {
    const saved = localStorage.getItem(this.key) as 'light' | 'dark' | 'system' | null;
    this.applyPreference(saved ?? 'system');
  }

  private applyPreference(pref: 'light' | 'dark' | 'system') {
    const root = document.querySelector('body')!;

    root.classList.remove('light', 'dark');

    if (pref === 'system') {
      const sysDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
      root.classList.add(sysDark ? 'dark' : 'light');
      return;
    }

    root.classList.add(pref);
  }
}