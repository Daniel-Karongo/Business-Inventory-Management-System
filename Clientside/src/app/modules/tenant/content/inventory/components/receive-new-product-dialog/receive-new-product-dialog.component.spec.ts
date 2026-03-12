import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ReceiveNewProductDialogComponent } from './receive-new-product-dialog.component';

describe('ReceiveNewProductDialogComponent', () => {
  let component: ReceiveNewProductDialogComponent;
  let fixture: ComponentFixture<ReceiveNewProductDialogComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ReceiveNewProductDialogComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ReceiveNewProductDialogComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
