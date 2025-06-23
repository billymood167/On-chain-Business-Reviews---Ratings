import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';

Clarinet.test({
  name: "Ensure business registration works",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get("deployer")!;
    const businessName = "Test Business";

    let block = chain.mineBlock([
      Tx.contractCall(
        "On-chain-Business-Reviews---Ratings",
        "register-business",
        [types.ascii(businessName)],
        deployer.address
      )
    ]);

    block.receipts[0].result.expectOk().expectUint(1);
  }
});

Clarinet.test({
  name: "Ensure review submission requires purchase",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const user = accounts.get("wallet_1")!;

    let block = chain.mineBlock([
      Tx.contractCall(
        "On-chain-Business-Reviews---Ratings",
        "submit-review",
        [types.uint(1), types.uint(5), types.ascii("Great service!")],
        user.address
      )
    ]);

    block.receipts[0].result.expectErr().expectUint(105);
  }
});

Clarinet.test({
  name: "Ensure only DAO can flag reviews",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const user = accounts.get("wallet_1")!;

    let block = chain.mineBlock([
      Tx.contractCall(
        "On-chain-Business-Reviews---Ratings",
        "flag-review",
        [types.uint(1)],
        user.address
      )
    ]);

    block.receipts[0].result.expectErr().expectUint(100);
  }
});
