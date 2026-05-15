import {
  Component,
  OnInit,
  inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
  FormBuilder,
  ReactiveFormsModule,
  Validators
} from '@angular/forms';

import {
  ActivatedRoute
} from '@angular/router';

import {
  finalize
} from 'rxjs';

import {
  MatButtonModule
} from '@angular/material/button';

import {
  MatFormFieldModule
} from '@angular/material/form-field';

import {
  MatInputModule
} from '@angular/material/input';

import {
  MatSlideToggleModule
} from '@angular/material/slide-toggle';

import {
  MatProgressSpinnerModule
} from '@angular/material/progress-spinner';

import {
  WorkflowCardComponent
} from '../../../../../../shared/layout/workflow-card/workflow-card.component';

import {
  BranchService
} from '../../services/branch.service';

import {
  BranchSecuritySettingsDTO
} from '../../models/branch.model';
import { LocationService } from '../../../../../../core/services/location.service';
import { MatSnackBar } from '@angular/material/snack-bar';
import { MatIconModule } from '@angular/material/icon';
import { MatTooltipModule } from '@angular/material/tooltip';

@Component({
  standalone: true,
  selector: 'app-branch-security',
  imports: [
    CommonModule,
    ReactiveFormsModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    MatSlideToggleModule,
    MatProgressSpinnerModule,
    MatTooltipModule,
    MatIconModule,
    WorkflowCardComponent
  ],
  templateUrl: './branch-security.component.html',
  styleUrls: ['./branch-security.component.scss']
})
export class BranchSecurityComponent
  implements OnInit {

  private fb = inject(FormBuilder);

  private route = inject(ActivatedRoute);

  private snackbar =
    inject(MatSnackBar);

  private locationService =
    inject(LocationService);

  private branchService =
    inject(BranchService);

  loading = true;

  detectingLocation = false;

  saving = false;

  branchId = '';

  form = this.fb.nonNullable.group({

    latitude: [
      null as number | null
    ],

    longitude: [
      null as number | null
    ],

    radiusMeters: [
      null as number | null,
      [
        Validators.min(1)
      ]
    ],

    enforceGeofence: [false],

    enforceDevice: [false]
  });

  ngOnInit(): void {

    const id =
      this.route.parent?.snapshot.paramMap.get('id');

    if (!id) {
      return;
    }

    this.branchId = id;

    this.branchService
      .getById(id)
      .subscribe({
        next: branch => {

          this.form.patchValue({
            latitude: branch.latitude,
            longitude: branch.longitude,
            radiusMeters: branch.radiusMeters,
            enforceGeofence:
              branch.enforceGeofence,
            enforceDevice:
              branch.enforceDevice
          });

          this.loading = false;
        },
        error: () => {
          this.loading = false;
        }
      });
  }

  async useCurrentLocation(): Promise<void> {

    if (this.detectingLocation) {
      return;
    }

    this.detectingLocation = true;

    try {

      const location =
        await this.locationService
          .getLocation();

      if (
        !location.latitude
        || !location.longitude
      ) {

        this.snackbar.open(
          'Unable to retrieve current location',
          'Close',
          {
            duration: 4000
          }
        );

        return;
      }

      this.form.patchValue({
        latitude: location.latitude,
        longitude: location.longitude
      });

      this.snackbar.open(
        'Current location detected',
        'Close',
        {
          duration: 3000
        }
      );

    } catch {

      this.snackbar.open(
        'Location detection failed',
        'Close',
        {
          duration: 4000
        }
      );

    } finally {

      this.detectingLocation = false;
    }
  }

  save(): void {

    if (
      this.form.invalid ||
      this.saving
    ) {
      this.form.markAllAsTouched();
      return;
    }

    this.saving = true;

    const payload:
      BranchSecuritySettingsDTO = {

      latitude:
        this.form.getRawValue().latitude,

      longitude:
        this.form.getRawValue().longitude,

      radiusMeters:
        this.form.getRawValue().radiusMeters,

      enforceGeofence:
        this.form.getRawValue().enforceGeofence,

      enforceDevice:
        this.form.getRawValue().enforceDevice
    };

    this.branchService
      .updateSecuritySettings(
        this.branchId,
        payload
      )
      .pipe(
        finalize(() => {
          this.saving = false;
        })
      )
      .subscribe({
        next: branch => {

          this.form.patchValue({
            latitude: branch.latitude,
            longitude: branch.longitude,
            radiusMeters:
              branch.radiusMeters,
            enforceGeofence:
              branch.enforceGeofence,
            enforceDevice:
              branch.enforceDevice
          });

          this.form.markAsPristine();

          this.snackbar.open(
            'Security settings updated',
            'Close',
            {
              duration: 3000
            }
          );
        },

        error: (err) => {

          const message =
            err?.error?.message
            || 'Failed to update security settings';

          this.snackbar.open(
            message,
            'Close',
            {
              duration: 4000
            }
          );
        }
      });
  }
}