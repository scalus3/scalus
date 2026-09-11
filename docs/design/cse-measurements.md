# CSE placement measurements

The tables below record commit `dacf54604`, before the subsequent constant-propagation fix.
That fix restores `List.singleton(1).init` to 400 memory / 48,100 CPU by simplifying through
retained constant bindings before deciding whether to share them. The historical regression
below describes the measured commit, not the fixed optimizer. Rebuilding the 23-validator
corpus after the fix leaves 22 scripts unchanged; the bilinear-accumulator allowlist grows
from 588 to 607 bytes, bringing the total from 32,241 to 32,260 bytes.

Baseline: `d4cf3b63a`. Measured on the JVM with Scala 3.3.8 using the existing default
compiler options and each test’s existing protocol parameters. CSE stays enabled; CCE’s
default remains unchanged. These measurements cover the complete CSE placement change,
including unique binders, innermost binding placement and collect-apply-repeat.

CSE and inlining now use the same profitability estimate. For n occurrences:

```
savedBits = (n - 1) * termBits(expr) - n * VarBits - 8
savedLovelace = savedBits * referencePricePerBit - (applyFee + lambdaFee + varFee)
```

The model assumes one execution of the binding and the first reference-script fee tier, using
the repository's mainnet reference parameters. It gives no credit for avoided computation.
Variable-index widths and byte alignment remain estimates; the removed exact-size calculator
is not needed. Twelve once-forced uses or six twice-forced uses first pass this estimate in the
focused constructor examples.

The inliner keeps exact occurrence counts and retains profitable shared values. Its safety rules
and eligible term kinds are unchanged. This preparatory refactor is committed separately as
`c87572e55`; the subsequent CSE change uses the same helper. The tables below measure the
combined result against the original baseline, including the updated budget assertions.

Compared with size-only selection, the shared fee model changes 20/23 blueprint hashes and adds
73 bytes (32168 -> 32241). This heuristic trades some size savings for estimated binding execution
costs. It does not guarantee lower actual runtime budgets or transaction fees; hot bindings can
execute more often than the model assumes.

## Generated blueprints

Both snapshots follow a fresh `scalusExamplesJVM/blueprint` run. Sizes count bytes in
`compiledCode`; they are not AST node counts.

Hashes changed: 21/23. Bytes: 32952 -> 32241 (-711).

| Validator | Before bytes | After bytes | Delta | Hash changed |
|---|---:|---:|---:|:---:|
| Hello Cardano | 149 | 149 | +0 | no |
| Constant-product AMM | 3144 | 2944 | -200 | yes |
| Auction | 2338 | 2246 | -92 | yes |
| Betting validator | 2043 | 1989 | -54 | yes |
| Bilinear-accumulator allowlist | 588 | 588 | +0 | no |
| Two-party escrow (CAPE) | 1079 | 1009 | -70 | yes |
| Crowdfunding campaign | 3243 | 3084 | -159 | yes |
| Crowdfunding donation minting policy | 774 | 766 | -8 | yes |
| Decentralized identity | 2407 | 2319 | -88 | yes |
| Editable NFT | 1081 | 1056 | -25 | yes |
| Three-party escrow smart contract | 1059 | 1030 | -29 | yes |
| Factory | 912 | 924 | +12 | yes |
| Hashed timelocked contract | 315 | 311 | -4 | yes |
| On-chain linked list | 3930 | 3870 | -60 | yes |
| Two-player lottery contract | 1439 | 1481 | +42 | yes |
| Naive Payment Splitter | 1627 | 1599 | -28 | yes |
| Optimized Payment Splitter | 1381 | 1378 | -3 | yes |
| Price bet | 933 | 945 | +12 | yes |
| Price oracle | 766 | 747 | -19 | yes |
| Simple Transfer contract | 835 | 844 | +9 | yes |
| Upgradeable proxy validator | 649 | 653 | +4 | yes |
| Vault | 1406 | 1460 | +54 | yes |
| Vesting validator | 854 | 849 | -5 | yes |

## Execution budgets

The table lists changed pinned assertions reached by the repository tests. A test can have
multiple entries because it evaluates several inputs or script purposes. Unchanged assertions
retain their existing pins. Memory and CPU are execution units, not CEK instruction counts.
Positive deltas are regressions. Pins were updated from actual evaluations, preserving the
behavioral assertions. A notable constant-folding regression is `List.singleton(1).init`: its
CPU budget rises from 48,100 to 3,567,612. Keeping a locally profitable binding can prevent
later simplification. This limitation is retained explicitly in the measurements; the shared fee
estimate is not a global optimization of the final program.

| Suite / test | Memory before | Memory after | CPU before | CPU after | CPU delta |
|---|---:|---:|---:|---:|---:|
| ClausifyTest / F1 | 32160481 | 30946389 | 9342486769 | 8989485267 | -3.78% |
| ClausifyTest / F2 | 40266933 | 38735065 | 11647653137 | 11215317059 | -3.71% |
| ClausifyTest / F3 | 108198882 | 104054622 | 31184827087 | 30032331361 | -3.70% |
| ClausifyTest / F4 | 141825115 | 136374755 | 40125180053 | 38763708327 | -3.39% |
| ClausifyTest / F5 | 523383799 | 503189547 | 150214598421 | 144597896099 | -3.74% |
| KnightsDataTest / 100_4x4 | 105913819 | 138777221 | 34326612662 | 44087137731 | +28.43% |
| KnightsDataTest / 100_6x6 | 190209925 | 246404128 | 81759003799 | 105396171993 | +28.91% |
| KnightsDataTest / 100_8x8 | 302081549 | 438507966 | 145220884524 | 212745799112 | +46.50% |
| G2AccumulatorTest / check membership one element | 64813 | 64313 | 1648999657 | 1648919657 | -0.00% |
| ValueTest / + multi-asset cancel | 94918 | 93474 | 24906455 | 25300775 | +1.58% |
| ValueTest / + multi-asset partial cancel lovelace | 49224 | 48852 | 13201542 | 13454702 | +1.92% |
| ValueTest / + multi-asset partial cancel token | 49224 | 48852 | 13201542 | 13454702 | +1.92% |
| ValueTest / - multi-asset cancel | 95372 | 93928 | 25526609 | 25920929 | +1.54% |
| ValueTest / - multi-asset partial cancel lovelace | 49657 | 49285 | 13544119 | 13797279 | +1.87% |
| ValueTest / - multi-asset partial cancel token | 49657 | 49285 | 13544119 | 13797279 | +1.87% |
| ValueTest / Eq vs toData: three policies equal via toData | 163520 | 160068 | 42419707 | 42961767 | +1.28% |
| ValueTest / Eq vs toData: two policies equal via Eq | 94009 | 92565 | 24496439 | 24890759 | +1.61% |
| ValueTest / Eq vs toData: two policies equal via toData | 94009 | 92565 | 24496439 | 24890759 | +1.61% |
| ValueTest / fromStrictlyAscendingListWithNonZeroAmounts two policies | 65647 | 62347 | 16079838 | 15781826 | -1.85% |
| TxInfoTest / budget: validFromOrFail and validToOrFail | 11174 | 10174 | 3868189 | 3708189 | -4.14% |
| TxInfoTest / budget: valueSpentFrom | 22452 | 21524 | 13211207 | 12751814 | -3.48% |
| HelloCardanoTest / Hello Cardano | 13758 | 13258 | 5383392 | 5303392 | -1.49% |
| AmmTest / budget: deposit | 137507 | 124331 | 60047191 | 52694994 | -12.24% |
| AmmTest / budget: redeem | 126028 | 115096 | 56208156 | 50040774 | -10.97% |
| AuctionValidatorTest / budget: end auction with winner | 135440 | 129655 | 49116296 | 47286714 | -3.72% |
| AuctionValidatorTest / budget: end auction without bids | 116537 | 109885 | 39126046 | 37145481 | -5.06% |
| AuctionValidatorTest / budget: first bid | 80399 | 74879 | 33351373 | 31628751 | -5.17% |
| AuctionValidatorTest / budget: outbid with refund | 98079 | 91155 | 40229211 | 38094369 | -5.31% |
| BettingTransactionTest / oracle announces winner after expiration | 103098 | 99142 | 39947168 | 38509598 | -3.60% |
| BettingTransactionTest / player2 joins bet before expiration | 115215 | 111919 | 45392080 | 44391698 | -2.20% |
| BettingValidatorTest / Verify that a bet can be properly initialized | 66051 | 62095 | 22235717 | 20919115 | -5.92% |
| BettingValidatorTest / Verify that both players can reclaim the pot after a timeout | 121785 | 119129 | 48067333 | 46842672 | -2.55% |
| BettingValidatorTest / Verify that player2 can join an existing bet | 108693 | 104633 | 40824573 | 39604453 | -2.99% |
| BettingValidatorTest / Verify that the oracle can announce winner and trigger payout | 99373 | 94885 | 37503666 | 35961508 | -4.11% |
| HtlcCapeTest / CAPE: claim_just_before_timeout | 43917 | 44945 | 18046963 | 18406297 | +1.99% |
| HtlcCapeTest / CAPE: claim_well_before_timeout | 43917 | 44945 | 18046963 | 18406297 | +1.99% |
| HtlcCapeTest / CAPE: refund_just_after_timeout | 40984 | 41812 | 16660141 | 16987475 | +1.96% |
| HtlcCapeTest / CAPE: refund_well_after_timeout | 40984 | 41812 | 16660141 | 16987475 | +1.96% |
| LinearVestingCapeTest / CAPE: full_unlock_after_period_end | 23422 | 23954 | 8983746 | 9145409 | +1.80% |
| LinearVestingCapeTest / CAPE: full_unlock_well_after | 23422 | 23954 | 8983746 | 9145409 | +1.80% |
| LinearVestingCapeTest / CAPE: partial_unlock_between_installments | 63888 | 64616 | 31013223 | 31324557 | +1.00% |
| LinearVestingCapeTest / CAPE: partial_unlock_first_installment | 63888 | 64616 | 31013223 | 31324557 | +1.00% |
| LinearVestingCapeTest / CAPE: partial_unlock_mid_vesting | 63888 | 64616 | 31013223 | 31324557 | +1.00% |
| LinearVestingCapeTest / CAPE: partial_unlock_near_end | 63888 | 64616 | 31013223 | 31324557 | +1.00% |
| TwoPartyEscrowCapeTest / CAPE: accept_successful | 41462 | 41362 | 17073255 | 17178503 | +0.62% |
| TwoPartyEscrowCapeTest / CAPE: accept_with_datum_attached | 41462 | 41362 | 17073255 | 17178503 | +0.62% |
| TwoPartyEscrowCapeTest / CAPE: accept_with_multiple_inputs | 41462 | 41362 | 17073255 | 17178503 | +0.62% |
| TwoPartyEscrowCapeTest / CAPE: accept_with_multiple_outputs_to_seller | 48843 | 48743 | 22165300 | 22270548 | +0.47% |
| TwoPartyEscrowCapeTest / CAPE: deposit_successful | 34090 | 33790 | 14152304 | 14104304 | -0.34% |
| TwoPartyEscrowCapeTest / CAPE: refund_after_exact_deadline | 50774 | 50442 | 20477652 | 20530156 | +0.26% |
| TwoPartyEscrowCapeTest / CAPE: refund_successful | 50774 | 50442 | 20477652 | 20530156 | +0.26% |
| TwoPartyEscrowCapeTest / CAPE: refund_with_datum_attached | 50774 | 50442 | 20477652 | 20530156 | +0.26% |
| TwoPartyEscrowCapeTest / CAPE: refund_with_multiple_inputs | 50774 | 50442 | 20477652 | 20530156 | +0.26% |
| TwoPartyEscrowCapeTest / CAPE: refund_with_multiple_outputs_to_buyer | 58155 | 57823 | 25569697 | 25622201 | +0.21% |
| EditableNftValidatorTest / Burn: successful burn removes both tokens | 73190 | 71726 | 25083592 | 24583293 | -1.99% |
| EscrowTest / Deposit: buyer deposits escrow amount | 102713 | 101353 | 50783003 | 50121742 | -1.30% |
| EscrowTest / Pay: buyer releases payment to seller | 95336 | 93310 | 41937697 | 40976679 | -2.29% |
| EscrowTest / Refund: seller refunds to buyer | 102784 | 105345 | 46412776 | 48401763 | +4.29% |
| HtlcTest / VALIDATOR: receiver reveals preimage before timeout | 26827 | 24731 | 11493882 | 10924152 | -4.96% |
| HtlcTest / committer reclaims after timeout | 23894 | 21598 | 8684711 | 8082981 | -6.93% |
| HtlcTest / receiver reveals preimage before timeout | 26827 | 24731 | 11493882 | 10924152 | -4.96% |
| LotteryValidatorTest / P1 concedes after P2 revealed - P2 gets pot | 48949 | 43669 | 19625192 | 18665584 | -4.89% |
| LotteryValidatorTest / P1 reveal succeeds at deadline (boundary) | 67653 | 59077 | 26921150 | 25554167 | -5.08% |
| LotteryValidatorTest / P1 reveals valid preimage from Empty state | 67653 | 59077 | 26921150 | 25554167 | -5.08% |
| LotteryValidatorTest / P1 reveals with even sum (32 + 16 = 48) after P2 revealed | 47099 | 42723 | 17949647 | 17264449 | -3.82% |
| LotteryValidatorTest / P1 timeout after P1 revealed but P2 didn't reveal | 51003 | 46299 | 19263644 | 18626483 | -3.31% |
| LotteryValidatorTest / P2 concedes after P1 revealed - P1 gets pot | 49181 | 43901 | 19693679 | 18734071 | -4.87% |
| LotteryValidatorTest / P2 reveals valid preimage from Empty state | 67885 | 58913 | 26989637 | 25389699 | -5.93% |
| LotteryValidatorTest / P2 reveals with even sum (32 + 16 = 48) | 47331 | 42955 | 18018134 | 17332936 | -3.80% |
| LotteryValidatorTest / P2 timeout after P2 revealed but P1 didn't reveal | 51235 | 46531 | 19332131 | 18694970 | -3.30% |
| NaivePaymentSplitterValidatorTest / Naive: success between 5 payees | 618512 | 607776 | 230768177 | 231328813 | +0.24% |
| NaivePaymentSplitterValidatorTest / Naive: success when payments are correctly split between 2 payees | 249752 | 250644 | 88887995 | 89108140 | +0.25% |
| NaivePaymentSplitterValidatorTest / Naive: success when payments are correctly split between 3 payees | 354369 | 353217 | 127989279 | 128611216 | +0.49% |
| NaivePaymentSplitterValidatorTest / Naive: success when payments are correctly split for a single payee | 160039 | 161143 | 56693290 | 56285754 | -0.72% |
| NaivePaymentSplitterValidatorTest / Naive: success when split equally and remainder compensates fee - o1 | 354369 | 353217 | 127989279 | 128611216 | +0.49% |
| NaivePaymentSplitterValidatorTest / Naive: success when split equally and remainder compensates fee - o2 | 354369 | 353217 | 127989279 | 128611216 | +0.49% |
| NaivePaymentSplitterValidatorTest / Naive: success when split equally and remainder compensates fee - o3 | 354369 | 353217 | 127989279 | 128611216 | +0.49% |
| NaivePaymentSplitterValidatorTest / Naive: success with multiple contract UTxOs | 412377 | 407329 | 157812883 | 155512360 | -1.46% |
| OptimizedPaymentSplitterValidatorTest / Optimized: budget comparison with multiple UTxOs | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: budget comparison with multiple UTxOs | 238162 | 243362 | 98780822 | 101027775 | +2.27% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success between 5 payees | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success between 5 payees | 262583 | 268639 | 108502558 | 111508297 | +2.77% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split between 2 payees | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split between 2 payees | 152477 | 155257 | 56929270 | 58186042 | +2.21% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split between 3 payees | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split between 3 payees | 187578 | 191450 | 72638870 | 74478631 | +2.53% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split for a single payee | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split for a single payee | 118977 | 120665 | 42701166 | 43374949 | +1.58% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o1 | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o1 | 187578 | 191450 | 72638870 | 74478631 | +2.53% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o2 | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o2 | 187578 | 191450 | 72638870 | 74478631 | +2.53% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o3 | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o3 | 187578 | 191450 | 72638870 | 74478631 | +2.53% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success with multiple contract UTxOs | 40464 | 39464 | 13068817 | 12850255 | -1.67% |
| OptimizedPaymentSplitterValidatorTest / Optimized: success with multiple contract UTxOs | 238162 | 243362 | 98780822 | 101027775 | +2.27% |
| PricebetValidatorTest / Oracle updates successfully | 52801 | 51005 | 22042144 | 21342961 | -3.17% |
| PricebetValidatorTest / Owner times out after deadline | 32687 | 31147 | 12218789 | 11946458 | -2.23% |
| PricebetValidatorTest / Player joins successfully | 61133 | 59825 | 27772872 | 27549301 | -0.80% |
| PricebetValidatorTest / Player wins with oracle rate above threshold | 67671 | 64339 | 24967736 | 24296257 | -2.69% |
| SimpleTransferValidatorTest / deposit | 77117 | 75813 | 32228326 | 32077948 | -0.47% |
| SimpleTransferValidatorTest / withdraw | 93012 | 90065 | 38903146 | 37207770 | -4.36% |
| SimpleTransferValidatorTest / withdraw all | 65019 | 62395 | 24203231 | 23285650 | -3.79% |
| VaultTransactionTest / vault deposit adds funds | 65739 | 64628 | 27831942 | 28441134 | +2.19% |
| VaultTransactionTest / vault finalization succeeds after withdrawal request | 67348 | 64380 | 27249144 | 26850807 | -1.46% |
| VaultTransactionTest / vault finalization succeeds after withdrawal request | 78151 | 77715 | 30536222 | 30563818 | +0.09% |
| VaultTransactionTest / vault withdrawal request | 67348 | 64380 | 27249144 | 26850807 | -1.46% |
| VestingTransactionTest / Full withdrawal after vesting ends | 103252 | 103548 | 45642020 | 45804859 | +0.36% |
| VestingTransactionTest / Partial 50% withdrawal at midpoint | 127595 | 128919 | 62434008 | 63011041 | +0.92% |
| ValueTest / flatten lovelace | 26680 | 27508 | 6562443 | 7007603 | +6.78% |
| ValueTest / flatten token | 26680 | 27508 | 6562443 | 7007603 | +6.78% |
| ValueTest / flatten multi-asset | 47596 | 49252 | 12178230 | 13068550 | +7.31% |
| KnightsTest / 100_4x4 | 119057995 | 146486004 | 24581834427 | 30533045087 | +24.21% |
| KnightsTest / 100_6x6 | 380412589 | 482516818 | 78244869412 | 99882987445 | +27.65% |
| KnightsTest / 100_8x8 | 734004267 | 1024545436 | 151408962577 | 211308454878 | +39.56% |
| TxInfoTest / budget: findContinuingOutputOrFail | 26150 | 26314 | 16235924 | 16418504 | +1.12% |
| TxInfoTest / budget: hasPaidTagged | 28459 | 28559 | 16022351 | 16038351 | +0.10% |
| ValueTest / budget: hasSameTokensAndAtLeastAda | 19770 | 20070 | 11522206 | 11570206 | +0.42% |
| ListTest / singleOrFail | 2164 | 1964 | 366777 | 334777 | -8.72% |
| ListTest / quicksort | 12592 | 12292 | 2270904 | 2222904 | -2.11% |
| ListTest / quicksort | 43001 | 42101 | 8173557 | 8029557 | -1.76% |
| ListTest / at | 1664 | 2660 | 276143 | 598470 | +116.72% |
| ListTest / at | 2860 | 2660 | 630470 | 598470 | -5.08% |
| ListTest / at | 1664 | 2660 | 276143 | 598470 | +116.72% |
| ListTest / !! | 1664 | 2660 | 276143 | 598470 | +116.72% |
| ListTest / !! | 2860 | 2660 | 630470 | 598470 | -5.08% |
| ListTest / !! | 1664 | 2660 | 276143 | 598470 | +116.72% |
| ListTest / groupBy | 28561 | 28261 | 6188745 | 6140745 | -0.78% |
| ListTest / groupBy | 55856 | 55256 | 12240144 | 12144144 | -0.78% |
| ListTest / groupMap | 29825 | 29525 | 6416788 | 6368788 | -0.75% |
| ListTest / groupMap | 58184 | 57584 | 12664230 | 12568230 | -0.76% |
| ListTest / groupMapReduce | 89058 | 87894 | 19752371 | 19431647 | -1.62% |
| ListTest / prependedAll - empty prepend single | 2032 | 2332 | 330033 | 378033 | +14.54% |
| ListTest / prependedAll - single prepend single | 4960 | 5260 | 936113 | 984113 | +5.13% |
| ListTest / prependedAll - two prepend single | 4960 | 5260 | 936113 | 984113 | +5.13% |
| ListTest / ++: - single ++: single | 4328 | 4960 | 814180 | 984613 | +20.93% |
| ListTest / ++: - two ++: single | 5860 | 6492 | 1126542 | 1296975 | +15.13% |
| ListTest / dropRight - single drop 1 | 14508 | 14344 | 3619925 | 3439112 | -4.99% |
| ListTest / dropRight - two drop 1 | 21057 | 20729 | 5540337 | 5178711 | -6.53% |
| ListTest / dropRight - two drop 2 | 20595 | 20267 | 5398291 | 5036665 | -6.70% |
| ListTest / takeRight - single take 1 | 12276 | 11880 | 3265743 | 3032186 | -7.15% |
| ListTest / takeRight - two take 1 | 15769 | 15673 | 4211868 | 4026311 | -4.41% |
| ListTest / takeRight - two take 3 | 19091 | 18299 | 5353953 | 4886839 | -8.72% |
| ListTest / init | 400 | 14844 | 48100 | 3567612 | +7317.07% |
| ListTest / init | 21557 | 21229 | 5668837 | 5307211 | -6.38% |
| SortedMapTest / singleOrFail | 8820 | 8320 | 1960097 | 1880097 | -4.08% |
| SortedMapTest / unsafeFromList | 13768 | 14260 | 3678255 | 4225995 | +14.89% |
| SortedMapTest / fromList | 43538 | 41966 | 10170953 | 10232113 | +0.60% |
| SortedMapTest / fromStrictlyAscendingList | 37134 | 37626 | 9142509 | 9690249 | +5.99% |
| SortedMapTest / union | 53058 | 64614 | 13793052 | 17868564 | +29.55% |
| SortedMapTest / union | 52226 | 63782 | 13640429 | 17715941 | +29.88% |
| SortedMapTest / union | 67289 | 69401 | 16193945 | 17675827 | +9.15% |
| SortedMapTest / unionMap | 54158 | 66614 | 13969052 | 18188564 | +30.21% |
| SortedMapTest / unionMap | 53326 | 65782 | 13816429 | 18035941 | +30.54% |
| SortedMapTest / unionMap | 68389 | 70801 | 16369945 | 17899827 | +9.35% |
| SortedMapTest / Eq | 17837 | 18165 | 4371486 | 4736646 | +8.35% |
| SortedMapTest / ToData <-> FromData | 26418 | 26910 | 6752175 | 7299915 | +8.11% |
| SortedMapTest / length | 31956 | 32448 | 7960422 | 8508162 | +6.88% |
| SortedMapTest / size | 31956 | 32448 | 7960422 | 8508162 | +6.88% |
| SortedMapTest / keys | 52218 | 52710 | 13678403 | 14226143 | +4.00% |
| SortedMapTest / values | 52914 | 53406 | 14019392 | 14567132 | +3.91% |
| SortedMapTest / mapValues | 45052 | 45544 | 11518582 | 12066322 | +4.76% |
| SortedMapTest / filter | 40501 | 40993 | 9975130 | 10522870 | +5.49% |
| SortedMapTest / filter | 40069 | 40561 | 9838768 | 10386508 | +5.57% |
| SortedMapTest / filterNot | 41533 | 42025 | 10207492 | 10755232 | +5.37% |
| SortedMapTest / filterNot | 41965 | 42457 | 10343854 | 10891594 | +5.30% |
| SortedMapTest / find | 7357 | 8321 | 1562338 | 1851062 | +18.48% |
| SortedMapTest / find | 6861 | 6561 | 1381674 | 1333674 | -3.47% |
| SortedMapTest / find | 37304 | 38460 | 9175364 | 9963828 | +8.59% |
| SortedMapTest / find | 40737 | 40329 | 9903651 | 10307391 | +4.08% |
| SortedMapTest / findMap | 40392 | 40884 | 10418647 | 10966387 | +5.26% |
| SortedMapTest / findMap | 43329 | 43821 | 10787100 | 11334840 | +5.08% |
| SortedMapTest / foldLeft | 14476 | 15008 | 3527562 | 3689225 | +4.58% |
| SortedMapTest / foldLeft | 59518 | 61606 | 15659827 | 16692556 | +6.59% |
| SortedMapTest / foldRight | 57818 | 58310 | 15387827 | 15935567 | +3.56% |
| SortedMapTest / get | 39046 | 39538 | 9119447 | 9667187 | +6.01% |
| SortedMapTest / get | 43748 | 44240 | 9909002 | 10456742 | +5.53% |
| SortedMapTest / getOrFail | 40906 | 41398 | 9803816 | 10351556 | +5.59% |
| SortedMapTest / at | 40906 | 41398 | 9803816 | 10351556 | +5.59% |
| SortedMapTest / insert | 2464 | 6688 | 460969 | 1577564 | +242.23% |
| SortedMapTest / insert | 6730 | 11586 | 1316655 | 2549994 | +93.67% |
| SortedMapTest / insert | 33452 | 32616 | 7792882 | 7837318 | +0.57% |
| SortedMapTest / delete | 29965 | 30293 | 6704853 | 7070013 | +5.45% |
| SortedMapTest / delete | 31197 | 31525 | 6969215 | 7334375 | +5.24% |

## Traced builds and transaction fees

The blueprint total does not describe every build configuration. With error traces enabled,
HelloCardano’s pinned script grows from 268 to 395 bytes. The pinned AMM transactions
also use error traces: deposit fees rise from 402,649 to 445,799 lovelace, and redeem fees
rise from 401,578 to 444,854 lovelace, despite reduced execution budgets. These are total
constructed transaction fees, not reference-script-only estimates.

## Scala 3.8.4 budget baselines

The version-specific benchmark and payment-splitter pins were also measured on Scala 3.8.4.
The table lists changed budget assertions; the same test suites are checked on Scala 3.9.0.

| Suite / test | Memory before | Memory after | CPU before | CPU after |
|---|---:|---:|---:|---:|
| ClausifyTest / F1 | 31654561 | 30440469 | 9161691905 | 8808690403 |
| ClausifyTest / F2 | 39738693 | 38206825 | 11458882029 | 11026545951 |
| ClausifyTest / F3 | 107010342 | 102866082 | 30760092094 | 29607596368 |
| ClausifyTest / F4 | 140636575 | 135186215 | 39700445060 | 38338973334 |
| ClausifyTest / F5 | 518681719 | 498487467 | 148534269685 | 142917567363 |
| KnightsDataTest / 100_4x4 | 91604479 | 118599581 | 29418319604 | 37428654403 |
| KnightsDataTest / 100_6x6 | 177146185 | 228570628 | 77277600681 | 99283079433 |
| KnightsDataTest / 100_8x8 | 287749649 | 416590146 | 140304943909 | 205235130003 |
| NaivePaymentSplitterValidatorTest / Naive: success between 5 payees | 602436 | 597648 | 225037506 | 227644202 |
| NaivePaymentSplitterValidatorTest / Naive: success when payments are correctly split between 2 payees | 239256 | 242904 | 85151385 | 86437850 |
| NaivePaymentSplitterValidatorTest / Naive: success when payments are correctly split between 3 payees | 342013 | 344681 | 123587982 | 125602819 |
| NaivePaymentSplitterValidatorTest / Naive: success when payments are correctly split for a single payee | 151403 | 154199 | 53621367 | 53953571 |
| NaivePaymentSplitterValidatorTest / Naive: success when split equally and remainder compensates fee - o1 | 342013 | 344681 | 123587982 | 125602819 |
| NaivePaymentSplitterValidatorTest / Naive: success when split equally and remainder compensates fee - o2 | 342013 | 344681 | 123587982 | 125602819 |
| NaivePaymentSplitterValidatorTest / Naive: success when split equally and remainder compensates fee - o3 | 342013 | 344681 | 123587982 | 125602819 |
| NaivePaymentSplitterValidatorTest / Naive: success with multiple contract UTxOs | 400021 | 398793 | 153411586 | 152503963 |
| OptimizedPaymentSplitterValidatorTest / Optimized: budget comparison with multiple UTxOs | 233246 | 238910 | 97038273 | 99515806 |
| OptimizedPaymentSplitterValidatorTest / Optimized: success between 5 payees | 257667 | 264187 | 106760009 | 109996328 |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split between 2 payees | 147561 | 150805 | 55186721 | 56674073 |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split between 3 payees | 182662 | 186998 | 70896321 | 72966662 |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when payments are correctly split for a single payee | 114061 | 116213 | 40958617 | 41862980 |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o1 | 182662 | 186998 | 70896321 | 72966662 |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o2 | 182662 | 186998 | 70896321 | 72966662 |
| OptimizedPaymentSplitterValidatorTest / Optimized: success when split equally and remainder compensates fee - o3 | 182662 | 186998 | 70896321 | 72966662 |
| OptimizedPaymentSplitterValidatorTest / Optimized: success with multiple contract UTxOs | 233246 | 238910 | 97038273 | 99515806 |

With Scala 3.8.4, the traced AMM deposit transaction fee changes from 400,405 to
443,555 lovelace, and redeem changes from 399,334 to 442,610 lovelace.




## Validation

The final `sbtn quick` passes, including 3,792 core JVM tests and 599 example tests.
The focused inliner/CSE/CCE/determinism suites pass 160 tests on JavaScript. Scala 3.8.4 and
3.9.0 each pass 75 inliner/CSE tests and 52 benchmark/example tests. MiMa passes; its new
exclusions cover only the inliner's private occurrence enum. The default Scala version is
restored to 3.3.8. A fresh blueprint snapshot exactly matches the table above.

## Reproduce

```sh
sbtn scalusExamplesJVM/blueprint
python3 scripts/cse-corpus.py snapshot target/cse-review/before.json
# Apply the CSE change, then rebuild.
sbtn scalusExamplesJVM/blueprint
python3 scripts/cse-corpus.py snapshot target/cse-review/after.json
python3 scripts/cse-corpus.py compare target/cse-review/before.json target/cse-review/after.json
sbtn quick
```
