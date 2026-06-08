import {
    OperationalExpense,
    OperationalExpenseSettlement
} from './operational-expense.model';

export interface OperationalExpenseWorkspace {
    expenseId: string;
    description: string;
    expenseAccountId: string;
    expenseAccountName: string;
    expense: OperationalExpense;
    settlements: OperationalExpenseSettlement[];
}