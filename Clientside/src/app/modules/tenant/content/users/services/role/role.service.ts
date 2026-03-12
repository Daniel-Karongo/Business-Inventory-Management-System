import { Injectable } from '@angular/core';
import { Role } from '../../models/role.model';
import { Observable, of } from 'rxjs';

@Injectable({ providedIn: 'root' })
export class RoleService {

  /** Match backend enum Role.java */
  private roles: Role[] = [
    { name: 'SUPERUSER' },
    { name: 'ADMIN' },
    { name: 'MANAGER' },
    { name: 'SUPERVISOR' },
    { name: 'EMPLOYEE' }
  ];

  list(): Observable<Role[]> {
    return of(this.roles);
  }
}