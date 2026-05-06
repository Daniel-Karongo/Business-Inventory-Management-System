export interface AllocationDetail {
    batchId: string;
    allocatedQuantity: number;
    availableQuantity: number;
    reservedQuantity: number;
    unitCost: number;
    totalCost: number;
    receivedAt: string;
}

export interface AllocationPreviewDTO {
    totalCost: number;
    allocations: AllocationDetail[];
}