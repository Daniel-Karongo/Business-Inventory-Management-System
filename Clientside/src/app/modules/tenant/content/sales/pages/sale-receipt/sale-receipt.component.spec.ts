import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SaleReceiptComponent } from './sale-receipt.component';

describe('SaleReceiptComponent', () => {
  let component: SaleReceiptComponent;
  let fixture: ComponentFixture<SaleReceiptComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [SaleReceiptComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SaleReceiptComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
