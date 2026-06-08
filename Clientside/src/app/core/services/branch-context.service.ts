import { Injectable, inject } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import { AuthService } from '../../modules/auth/services/auth.service';
import { BranchListItemDTO } from '../../modules/tenant/content/branches/models/branch.model';
import { BranchService } from '../../modules/tenant/content/branches/services/branch.service';

@Injectable({
  providedIn: 'root'
})
export class BranchContextService {

  private auth = inject(AuthService);
  private branchService = inject(BranchService);

  private readonly STORAGE_KEY =
    'active_branch_id';

  private readonly branchSubject =
    new BehaviorSubject<string | null>(null);

  readonly branch$ =
    this.branchSubject.asObservable();

  private readonly branchesSubject =
    new BehaviorSubject<BranchListItemDTO[]>([]);

  readonly branches$ =
    this.branchesSubject.asObservable();

  constructor() {

    const authBranch =
      this.auth
        .getSnapshot()
        ?.branchId
      ?? null;

    this.branchSubject.next(
      authBranch
    );

    this.refreshBranches();

  }

  setBranches(
    branches: BranchListItemDTO[]
  ): void {

    this.branchesSubject.next(
      branches
    );

    const current =
      this.branchSubject.value;

    if (
      current &&
      branches.some(
        b => b.id === current
      )
    ) {
      return;
    }

    const authBranch =
      this.auth
        .getSnapshot()
        ?.branchId
      ?? null;

    this.branchSubject.next(
      authBranch
    );

    if (authBranch) {

      localStorage.setItem(
        this.STORAGE_KEY,
        authBranch
      );

    }
  }

  get branches(): BranchListItemDTO[] {
    return this.branchesSubject.value;
  }

  get currentBranch(): string | null {
    return this.branchSubject.value;
  }

  setBranch(
    branchId: string
  ): void {

    localStorage.setItem(
      this.STORAGE_KEY,
      branchId
    );

    this.branchSubject.next(
      branchId
    );
  }

  resetToAuthBranch(): void {

    const authBranch =
      this.auth
        .getSnapshot()
        ?.branchId;

    localStorage.removeItem(
      this.STORAGE_KEY
    );

    this.branchSubject.next(
      authBranch ?? null
    );
  }

  refreshBranches(): void {

    this.branchService
      .getAllLegacy()
      .subscribe(branches => {

        this.setBranches(
          branches
        );

      });

  }
}