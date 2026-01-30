import {
    Component,
    OnInit,
    ChangeDetectionStrategy,
    inject
} from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonModule } from '@angular/material/button';
import { MatSelectModule } from '@angular/material/select';
import { MatDialog } from '@angular/material/dialog';

import { AclPermissionConditionsService } from './acl-permission-conditions.service';

interface PermissionVM {
    id: string;
    code: string;
    label: string;
    module: string;
}

@Component({
    selector: 'app-acl-permission-conditions',
    standalone: true,
    changeDetection: ChangeDetectionStrategy.OnPush,
    imports: [CommonModule, MatButtonModule, MatSelectModule],
    templateUrl: './acl-permission-conditions.component.html',
    styleUrls: ['./acl-permission-conditions.component.scss']
})
export class AclPermissionConditionsComponent implements OnInit {

    private svc = inject(AclPermissionConditionsService);
    private dialog = inject(MatDialog);

    roles: any[] = [];
    permissions: PermissionVM[] = [];

    /** Pre-grouped, immutable */
    permissionGroups: { module: string; items: PermissionVM[] }[] = [];

    selectedPermission?: PermissionVM;
    selectedRole?: any;

    conditions: any[] = [];
    editMode = false;

    private readonly ROLE_ORDER = [
        'SUPERUSER',
        'ADMIN',
        'MANAGER',
        'SUPERVISOR',
        'EMPLOYEE'
    ];

    loadingPermissions = true;
    loadingRoles = true;
    loadingConditions = false;

    ngOnInit() {
        this.loadRoles();
        this.loadPermissions();
    }

    private loadRoles() {
        this.loadingRoles = true;

        this.svc.listRoles().subscribe(r => {
            this.roles = [...r].sort(
                (a, b) =>
                    this.ROLE_ORDER.indexOf(a.name) -
                    this.ROLE_ORDER.indexOf(b.name)
            );
            this.loadingRoles = false;
        });
    }

    private loadPermissions() {
        this.loadingPermissions = true;

        this.svc.listPermissions().subscribe(raw => {
            const perms: PermissionVM[] = raw.map(p => ({
                id: p.id,
                code: p.code,
                module: this.extractModule(p.code),
                label: this.humanize(p.code)
            }));

            // ✅ STABLE SORT: module → label
            perms.sort((a, b) =>
                a.module === b.module
                    ? a.label.localeCompare(b.label)
                    : a.module.localeCompare(b.module)
            );

            this.permissionGroups = this.buildGroups(perms);
            this.loadingPermissions = false;
        });
    }
    
    loadConditions() {
        if (!this.selectedPermission || !this.selectedRole) return;

        this.loadingConditions = true;
        this.conditions = [];

        this.svc.listConditions(
            this.selectedPermission.id,
            this.selectedRole.name
        ).subscribe(c => {
            this.conditions = c;
            this.loadingConditions = false;
        });
    }

    enableEdit() {
        this.editMode = true;
    }

    cancelEdit() {
        this.editMode = false;
        this.loadConditions();
    }

    addCondition() {
        // Replace later with dialog — non-blocking
        const param = prompt('Request parameter');
        if (!param) return;

        const value = prompt('Expected value');
        if (value === null) return;

        this.svc
            .addCondition(this.selectedPermission!.id, {
                role: this.selectedRole.name,
                param,
                operator: 'EQ',
                value
            })
            .subscribe(() => this.loadConditions());
    }

    deleteCondition(c: any) {
        const reason = prompt('Reason for removal');
        if (!reason) return;

        this.svc
            .softDeleteCondition(c.id, reason)
            .subscribe(() => this.loadConditions());
    }

    /* =========================
       PURE HELPERS (RUN ONCE)
       ========================= */

    private buildGroups(perms: PermissionVM[]) {
        const map = new Map<string, PermissionVM[]>();

        for (const p of perms) {
            if (!map.has(p.module)) {
                map.set(p.module, []);
            }
            map.get(p.module)!.push(p);
        }

        return [...map.entries()]
            .sort(([a], [b]) => a.localeCompare(b))
            .map(([module, items]) => ({
                module,
                items
            }));
    }

    private extractModule(code: string): string {
        const m = code.match(/API_[A-Z]+_API_([^_]+)/);
        return m ? m[1] : 'OTHER';
    }

    private humanize(code: string): string {
        return code
            .replace(/^API_[A-Z]+_API_/, '')
            .replace(/_/g, ' ')
            .toLowerCase()
            .replace(/\b\w/g, c => c.toUpperCase());
    }
}