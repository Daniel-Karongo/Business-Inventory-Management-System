import { Product } from "../../products/parent/models/product.model";

export interface Supplier {
  id: string;
  name: string;
  region?: string;
  rating?: number;
  email: string[];
  phoneNumber: string[];
  deleted: boolean;
  createdAt: string;
  createdByUsername?: string;

  lastUpdatedAt?: string;
  lastUpdatedByUsername?: string;

  categories: {
    id: number;
    name: string;
  }[];

  products?: {
    id: string;
    name: string;
  }[];

  files: {
    name: string;
    description: string;
    url: string;
    image: boolean;
  }[];
}

/* ================= IMAGES ================= */

export interface SupplierImage {
  id: string;

  fileName: string;
  filePath?: string;        // never rendered
  description?: string;

  deleted: boolean;
  uploadedAt: string;
}

/* ================= AUDITS ================= */

/* ============================================================
   SUPPLIER AUDIT
============================================================ */

export interface SupplierAudit {
  id: string;

  supplierId: string;
  supplierName: string;

  action: string;
  fieldChanged?: string;

  oldValue?: string;
  newValue?: string;

  reason?: string;
  performedBy?: string;

  timestamp: string;
}

/* ============================================================
   IMAGE AUDIT
============================================================ */

export interface SupplierImageAudit {
  id: string;

  supplierId: string;
  supplierName: string;

  fileName: string;
  action: 'UPLOAD' | 'DELETE' | 'RESTORE';

  reason?: string;
  performedBy?: string;

  timestamp: string;
}

export interface SupplierImageAudit {
  id: string;

  supplierId: string;
  supplierName: string;

  fileName: string;
  action: 'UPLOAD' | 'DELETE' | 'RESTORE';

  reason?: string;
  performedBy?: string;

  timestamp: string;
}

export interface SupplierMinimalDTO {
  id: string;
  name: string;
}

/* ============================================================
   PRODUCTS (READ ONLY)
============================================================ */

export interface SupplierProduct extends Product {
  // no extension yet — explicit alias for clarity
}