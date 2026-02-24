import { Injectable } from '@angular/core';
import { MatIconRegistry } from '@angular/material/icon';
import { DomSanitizer } from '@angular/platform-browser';

@Injectable({ providedIn: 'root' })
export class IconLoader {
  constructor(
    private registry: MatIconRegistry,
    private sanitizer: DomSanitizer
  ) { }

  load() {
    const icons = [
      'dashboard',
      'inventory',
      'products',
      'categories',
      'suppliers',
      'sales',
      'reports',
      'payments',
      'customers',
      'finance',
      'accounts',
      'users',
      'branches',
      'departments',
      'access-control',
      'budget',
      'tax',
      'payments'
    ];

    icons.forEach(name =>
      this.registry.addSvgIcon(
        name,
        this.sanitizer.bypassSecurityTrustResourceUrl(`assets/icons/${name}.svg`)
      )
    );
  }
}