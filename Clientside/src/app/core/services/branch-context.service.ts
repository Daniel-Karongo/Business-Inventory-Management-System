import { Injectable, inject } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import { AuthService } from '../../modules/auth/services/auth.service';

@Injectable({ providedIn: 'root' })
export class BranchContextService {

  private auth = inject(AuthService);

  private readonly STORAGE_KEY = 'active_branch_id';

  private branchSubject = new BehaviorSubject<string | null>(null);
  branch$ = this.branchSubject.asObservable();

  constructor() {
    const saved = localStorage.getItem(this.STORAGE_KEY);
    const authBranch = this.auth.getSnapshot()?.branchId ?? null;

    // Priority:
    // 1. Saved override (if admin)
    // 2. Auth branch
    this.branchSubject.next(saved ?? authBranch);
  }

  get currentBranch(): string | null {
    return this.branchSubject.value;
  }

  setBranch(branchId: string | null) {

    const role = this.auth.getSnapshot()?.role;

    // Employees cannot override branch
    if (role === 'EMPLOYEE') {
      return;
    }

    if (branchId) {
      localStorage.setItem(this.STORAGE_KEY, branchId);
    } else {
      localStorage.removeItem(this.STORAGE_KEY);
    }

    this.branchSubject.next(branchId);
  }

  resetToAuthBranch() {
    const authBranch = this.auth.getSnapshot()?.branchId ?? null;
    this.branchSubject.next(authBranch);
  }
}