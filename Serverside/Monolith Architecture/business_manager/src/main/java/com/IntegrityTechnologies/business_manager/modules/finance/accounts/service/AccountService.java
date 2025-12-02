package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.Account;

import java.util.List;
import java.util.UUID;

public interface AccountService {

    Account createAccount(Account account);

    Account getAccount(UUID id);

    List<Account> listAccounts();
}