import { ComponentFixture, TestBed } from '@angular/core/testing';

import { LedgerHomeComponent } from './ledger-home.component';

describe('LedgerHomeComponent', () => {
  let component: LedgerHomeComponent;
  let fixture: ComponentFixture<LedgerHomeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [LedgerHomeComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(LedgerHomeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
