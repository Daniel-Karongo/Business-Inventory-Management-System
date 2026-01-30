import { TestBed } from '@angular/core/testing';

import { AclAdminService } from './acl-admin.service';

describe('AclAdminService', () => {
  let service: AclAdminService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(AclAdminService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
