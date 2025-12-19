export function filterBy<T>(
    items: T[],
    value: string,
    accessor: (i: T) => string
): T[] {
    const v = (value || '').toLowerCase();
    return items.filter(i => accessor(i).toLowerCase().includes(v));
}