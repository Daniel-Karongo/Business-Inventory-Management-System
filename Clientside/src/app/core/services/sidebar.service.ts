import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class SidebarService {

  /* =========================================
     DESKTOP COLLAPSE
  ========================================= */

  private collapsedSubject =
    new BehaviorSubject<boolean>(false);

  readonly isCollapsed$ =
    this.collapsedSubject.asObservable();

  /* =========================================
     COLLAPSED RAIL EXPANSION
  ========================================= */

  private railExpandedSubject =
    new BehaviorSubject<boolean>(false);

  readonly railExpanded$ =
    this.railExpandedSubject.asObservable();

  /* =========================================
     MOBILE DRAWER
  ========================================= */

  private mobileOpenSubject =
    new BehaviorSubject<boolean>(false);

  readonly mobileOpen$ =
    this.mobileOpenSubject.asObservable();

  /* =========================================
     MOBILE MODE
  ========================================= */

  private mobileModeSubject =
    new BehaviorSubject<boolean>(false);

  readonly mobileMode$ =
    this.mobileModeSubject.asObservable();

  /* =========================================
     SNAPSHOTS
  ========================================= */

  get collapsedSnapshot(): boolean {
    return this.collapsedSubject.value;
  }

  get railExpandedSnapshot(): boolean {
    return this.railExpandedSubject.value;
  }

  get mobileSnapshot(): boolean {
    return this.mobileOpenSubject.value;
  }

  get mobileModeSnapshot(): boolean {
    return this.mobileModeSubject.value;
  }

  /* =========================================
     MOBILE MODE
  ========================================= */

  setMobileMode(value: boolean) {

    this.mobileModeSubject.next(value);

    if (value) {

      /* MOBILE NEVER USES COLLAPSED RAIL */

      this.collapsedSubject.next(false);
      this.railExpandedSubject.next(false);

      return;
    }

    this.mobileOpenSubject.next(false);
  }

  /* =========================================
     DESKTOP
  ========================================= */

  toggleDesktopCollapsed() {

    if (this.mobileModeSnapshot) {
      return;
    }

    const next =
      !this.collapsedSubject.value;

    this.collapsedSubject.next(next);

    if (!next) {
      this.railExpandedSubject.next(false);
    }
  }

  setDesktopCollapsed(value: boolean) {

    if (this.mobileModeSnapshot) {
      return;
    }

    this.collapsedSubject.next(value);

    if (!value) {
      this.railExpandedSubject.next(false);
    }
  }

  expandRail() {

    if (this.mobileModeSnapshot) {
      return;
    }

    this.railExpandedSubject.next(true);
  }

  collapseRail() {
    this.railExpandedSubject.next(false);
  }

  resetRail() {
    this.railExpandedSubject.next(false);
  }

  /* =========================================
     MOBILE
  ========================================= */

  openMobile() {

    if (!this.mobileModeSnapshot) {
      return;
    }

    this.mobileOpenSubject.next(true);
  }

  closeMobile() {
    this.mobileOpenSubject.next(false);
  }

  toggleMobile() {

    if (!this.mobileModeSnapshot) {
      return;
    }

    this.mobileOpenSubject.next(
      !this.mobileOpenSubject.value
    );
  }
}