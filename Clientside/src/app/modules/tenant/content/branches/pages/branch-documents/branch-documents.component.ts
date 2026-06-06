import { CommonModule } from '@angular/common';
import { Component, OnInit, ViewChild } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { WorkflowCardComponent } from '../../../../../../shared/layout/workflow-card/workflow-card.component';
import { EntityImageManagerComponent } from '../../../../../../shared/components/entity-image-manager/entity-image-manager.component';
import { BranchService } from '../../services/branch.service';
import { BranchDocumentAdapter } from '../../services/branch-document.adapter';
import { BranchWorkspaceStateService } from '../../services/branch-workspace-state.service';

@Component({
    standalone: true,
    selector: 'app-branch-documents',
    imports: [
        CommonModule,
        WorkflowCardComponent,
        EntityImageManagerComponent
    ],
    templateUrl: './branch-documents.component.html',
    styleUrls: ['./branch-documents.component.scss']
})
export class BranchDocumentsComponent implements OnInit {

    @ViewChild(EntityImageManagerComponent)
    imageManager?: EntityImageManagerComponent;

    branchId!: string;

    readonly adapter;

    constructor(
        private route: ActivatedRoute,
        private service: BranchService,
        private workspaceState: BranchWorkspaceStateService
    ) {
        this.adapter = BranchDocumentAdapter(
            this.service,
            () => this.refreshBranch()
        );
    }

    ngOnInit(): void {
        const id =
            this.route.parent?.snapshot.paramMap.get('id');

        if (id) {
            this.branchId = id;
        }
    }

    ngAfterViewInit(): void {
        this.workspaceState
            .registerDocumentsComponent(this);
    }

    private refreshBranch(): void {
        this.service
            .getById(this.branchId)
            .subscribe(branch => {
                this.workspaceState.setBranch(branch);
            });
    }

    public openUploadDialog(): void {
        this.imageManager?.openUploadDialog();
    }
}