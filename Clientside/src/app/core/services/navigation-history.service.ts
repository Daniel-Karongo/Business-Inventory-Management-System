import { Injectable } from "@angular/core";

@Injectable({ providedIn: 'root' })
export class NavigationHistoryService {
  private history: string[] = [];

  push(url: string) {
    // prevent duplicates
    if (this.history[this.history.length - 1] !== url) {
      this.history.push(url);
    }
  }

  back(): string | null {
    this.history.pop(); // remove current
    return this.history.pop() ?? null; // return previous
  }
}