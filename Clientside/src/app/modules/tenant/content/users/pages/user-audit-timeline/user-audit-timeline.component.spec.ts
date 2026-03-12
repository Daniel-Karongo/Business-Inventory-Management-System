import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UserAuditTimelineComponent } from './user-audit-timeline.component';

describe('UserAuditTimelineComponent', () => {
  let component: UserAuditTimelineComponent;
  let fixture: ComponentFixture<UserAuditTimelineComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [UserAuditTimelineComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(UserAuditTimelineComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
