import { CommonModule } from '@angular/common';
import {
    Component,
    Input
} from '@angular/core';

import {
    PackagingDTO
} from '../../../../models/packaging.model';

@Component({
    selector: 'app-packaging-conversion-table',
    standalone: true,
    imports: [CommonModule],
    templateUrl: './packaging-conversion-table.component.html',
    styleUrls: ['./packaging-conversion-table.component.scss']
})
export class PackagingConversionTableComponent {

    @Input({ required: true }) packaging: PackagingDTO[] = [];
}