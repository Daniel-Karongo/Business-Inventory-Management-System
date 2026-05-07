import {
    ChangeDetectionStrategy,
    Component,
    inject
} from '@angular/core';

import { CommonModule } from '@angular/common';

import {
    OnboardingProductStepComponent
} from '../../components/onboarding-product-step/onboarding-product-step.component';
import {
    OnboardingProgressComponent
} from '../../components/onboarding-progress/onboarding-progress.component';

import {
    OnboardingActionsComponent
} from '../../components/onboarding-actions/onboarding-actions.component';

import {
    OnboardingState
} from '../../state/onboarding.state';

import {
    StockOnboardingFacadeService
} from '../../services/stock-onboarding-facade.service';
import { PageShellComponent } from '../../../../../../shared/layout/page-shell/page-shell.component';

import {
    OnboardingVariantStepComponent
} from '../../components/onboarding-variant-step/onboarding-variant-step.component';

import {
    OnboardingPackagingStepComponent
} from '../../components/onboarding-packaging-step/onboarding-packaging-step.component';

import {
    OnboardingPricingStepComponent
} from '../../components/onboarding-pricing-step/onboarding-pricing-step.component';

import {
    OnboardingSupplierStepComponent
} from '../../components/onboarding-supplier-step/onboarding-supplier-step.component';

import {
    OnboardingReviewStepComponent
} from '../../components/onboarding-review-step/onboarding-review-step.component';
import { WorkflowShellComponent } from '../../../../../../shared/layout/workflow-shell/workflow-shell.component';
import { WorkflowCardComponent } from '../../../../../../shared/layout/workflow-card/workflow-card.component';

@Component({
    selector:
        'app-onboarding-create-page',

    standalone: true,

    imports: [
        CommonModule,
        PageShellComponent,
        OnboardingProgressComponent,
        OnboardingActionsComponent,
        OnboardingProductStepComponent,
        OnboardingVariantStepComponent,
        OnboardingPackagingStepComponent,
        OnboardingPricingStepComponent,
        OnboardingSupplierStepComponent,
        OnboardingReviewStepComponent,
        WorkflowShellComponent,
        WorkflowCardComponent
    ],

    templateUrl:
        './onboarding-create-page.component.html',

    styleUrls: [
        './onboarding-create-page.component.scss'
    ],

    providers: [
        OnboardingState,
        StockOnboardingFacadeService
    ],

    changeDetection:
        ChangeDetectionStrategy.OnPush
})
export class OnboardingCreatePageComponent {

    readonly facade =
        inject(
            StockOnboardingFacadeService
        );

    readonly state =
        this.facade.state;

}