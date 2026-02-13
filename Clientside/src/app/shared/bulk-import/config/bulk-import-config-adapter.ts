import { FormArray, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { BulkImportConfig } from '../models/bulk-import-config.model';

export class BulkImportConfigAdapter {

  static createForm(
    fb: FormBuilder,
    config: BulkImportConfig<any, any, any>
  ): FormGroup {

    const rows = fb.array([]);

    return fb.group({
      dryRun: [config.defaultOptions?.dryRun ?? true],
      rows
    });
  }

  static createRow(
    fb: FormBuilder,
    config: BulkImportConfig<any, any, any>,
    data?: any
  ): FormGroup {

    const group: any = {};

    config.fields.forEach(f => {
      group[f.name] = [
        data?.[f.name] ?? f.defaultValue ?? '',
        f.required ? Validators.required : []
      ];
    });

    // 🔥 NEW — files live inside row form
    group['files'] = [data?.files ?? []];

    group['_error'] = [''];

    return fb.group(group);
  }

  static addRow(
    fb: FormBuilder,
    form: FormGroup,
    config: BulkImportConfig<any, any, any>,
    data?: any
  ) {
    (form.get('rows') as FormArray)
      .push(this.createRow(fb, config, data));
  }
}