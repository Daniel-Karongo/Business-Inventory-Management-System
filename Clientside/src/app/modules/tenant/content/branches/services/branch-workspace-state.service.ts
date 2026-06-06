import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import { BranchDetailsDTO } from '../models/branch.model';
import { BranchDocumentsComponent } from '../pages/branch-documents/branch-documents.component';

@Injectable({
    providedIn: 'root'
})
export class BranchWorkspaceStateService {
    private readonly branchSubject =
        new BehaviorSubject<BranchDetailsDTO | null>(null);

    readonly branch$ =
        this.branchSubject.asObservable();

    private documentsComponent?: BranchDocumentsComponent;

    registerDocumentsComponent(
        component: BranchDocumentsComponent
    ): void {
        this.documentsComponent = component;
    }

    openUploadDialog(): void {
        this.documentsComponent?.openUploadDialog();
    }

    get current(): BranchDetailsDTO | null {
        return this.branchSubject.value;
    }

    setBranch(
        branch: BranchDetailsDTO
    ): void {
        this.branchSubject.next(branch);
    }

    clear(): void {
        this.branchSubject.next(null);
    }
}