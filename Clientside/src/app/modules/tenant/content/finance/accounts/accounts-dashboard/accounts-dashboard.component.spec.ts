import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AccountsDashboardComponent } from './accounts-dashboard.component';

describe('AccountsDashboardComponent', () => {
  let component: AccountsDashboardComponent;
  let fixture: ComponentFixture<AccountsDashboardComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [AccountsDashboardComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(AccountsDashboardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
