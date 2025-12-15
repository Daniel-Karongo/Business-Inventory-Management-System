import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UserRollcallsComponent } from './user-rollcalls.component';

describe('UserRollcallsComponent', () => {
  let component: UserRollcallsComponent;
  let fixture: ComponentFixture<UserRollcallsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [UserRollcallsComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(UserRollcallsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
