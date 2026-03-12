export type CustomerType = 'INDIVIDUAL' | 'COMPANY';
export type Gender = 'MALE' | 'FEMALE' | 'OTHER';

export interface CustomerRequest {
  name: string;
  type: CustomerType;
  gender?: Gender | null;
  phoneNumbers?: string[];
  emailAddresses?: string[];
  address?: string;
  notes?: string;
}

export interface CustomerResponse {
  id: string;
  name: string;
  type: CustomerType;
  gender?: Gender | null;
  phoneNumbers?: string[];
  email?: string[];
  address?: string;
  notes?: string;
  deleted?: boolean;
  createdAt?: string;
  updatedAt?: string;
}