# Thesis Data Work — Pre-Writing Audit

**Subject:** AI-Related Disclosure in Earnings Calls: Investor Attention, Valuation Effects, and Subsequent Reversal
**Author:** Duy Dam
**Audit date:** 2026-04-25
**Auditor scope:** End-to-end review of pipeline, measurement, identification, and statistical robustness, plus a side-by-side comparison of current results against the proposal and the in-progress results memo.

---

## Bottom line

The data work is **defensible and ready to write up**, but the headline story you originally proposed is not the story the data tell. The pipeline is sound, the measurement is reasonable, and the statistical apparatus is solid. The honest finding is that **AI tone moves prices in the short run, AI substance moves prices in the long run, and the AI-washing reversal you hypothesised does not appear in this sample.** That is a coherent, publishable narrative — but it requires you to update the framing in the proposal/draft, fix several stale numbers and label inconsistencies in the working memo, and hedge appropriately on causality and on a few weak corners of the evidence.

You should proceed to writing. You should *not* proceed to writing without the corrections in §6 below.

---

## 1. Workflow integrity

The pipeline is end-to-end traceable. Raw transcripts and price/IBES/Compustat pulls live in `data/raw/`; processed panels in `data/processed/`; analysis outputs in `output/`. The Python text-processing modules (`ai_dictionary.py`, `text_processing.py`, `lm_sentiment.py`, `metadata.py`, `pipeline.py`) cleanly separate concerns: dictionary definitions, presentation/Q&A segmentation, LM sentiment, header parsing, and the assembly step. The two notebooks (`data_merge_2.0.ipynb` for returns and event windows; `control_var.ipynb` for SUE/coverage/ROA/B-M/size/leverage) are well structured, and the R notebook (`panel_regression_2.0.R`) executes the regressions and robustness exercises without ad-hoc patching. Match rates are high (SUE 99.3%, NUMEST 99.9%, Compustat 99.9%), with documented dropouts (META lacking IBES coverage; one duplicate UNP record removed). The final analytical sample is 1,546 firm-quarter observations, 1,533 of which carry complete controls. This is a near-balanced panel of ~98 S&P 100 firms over roughly Q4 2021 through Q1 2026.

Verdict: nothing in the workflow needs rework. It is reproducible and the lineage from raw text to regression coefficient is clear.

## 2. Measurement validity

**AI dictionary.** The split between *AI core* (LLMs, machine learning, NLP, computer vision, named technologies) and *AI adjacent* (automation, analytics, robotics, digital transformation, generic "AI") is conceptually defensible and matches recent literature on AI disclosure. Per-1,000-word normalisation is the right scaling choice. Two caveats worth a sentence in the methodology section: (i) dictionary-based methods cannot detect *implicit* AI references, and (ii) the same word ("models", "neural") can carry different meaning across industries — your firm fixed effect partially absorbs this but does not eliminate it.

**LM tone and uncertainty.** The constructions are standard: tone = (pos − neg) / (pos + neg), uncertainty = unc / total\_words. Bounds are validated. The presentation/Q&A split is regex-driven and clean.

**Outcome variables.** Short-run is a 3-trading-day CAR around the call date (event\_trading\_day −1 through +1) using market-adjusted abnormal returns AR = DlyRet − vwretd. Long-run is the daily AR sum from event\_trading\_day+1 to one trading day before the next call. Two flags here:

- **Label inconsistency.** The variable is `car_m1_p1` and you report it in the regression tables as "CAR [−1,+1]". That is correct. But your descriptive memo and the `summary_stats.txt` label call it "CAR [−1,+3]". The window is three *trading days* (−1, 0, +1), not three *calendar days from* −1 to +3. Pick one label and use it consistently. The cleanest is "3-day CAR [−1,+1]".
- **Long-run construction.** You compute the simple sum of daily ARs, not a buy-and-hold abnormal return (BHAR). For a 60–90 trading-day horizon this is a reasonable approximation but it is not a BHAR. State this explicitly in the methods and cite Lyon, Barber, and Tsai (1999) or similar on the trade-off; you will pre-empt a reviewer asking why you did not use BHAR.

## 3. Identification and what you can claim

Two-way fixed effects (firm + calendar-quarter) is the right specification, and the Hausman test confirms it: all six outcome×segment specifications reject random effects at p<0.001. Firm FE absorbs time-invariant firm characteristics (industry, leadership, business model) and time FE absorbs market-wide shocks (the 2022 drawdown, the 2023 generative-AI rally, etc.). Firm-clustered HC1 standard errors are appropriate given the panel structure.

What this gets you is a **within-firm, between-quarter** estimate: holding the firm constant and controlling for the macro quarter, does talking more about AI on a given call associate with abnormal returns? That is association, not causation. The mention of AI is endogenous to firm strategy and to managerial private information about future cashflows. You cannot rule out that firms which already have something real to announce simply talk more about AI. **Frame the long-run AI-core coefficient as "consistent with" or "associated with", not as a causal effect.** A sentence acknowledging the simultaneity concern, and noting that an instrument or quasi-experimental shock would be required for causal identification, is sufficient.

## 4. Statistical robustness

Defensible across the board. Breusch-Pagan rejects homoskedasticity in every model — that justifies HC1. VIFs are all below 2 (well under any concerning threshold). Maximum pairwise |r| among regressors is roughly 0.49, no |r|>0.6 — collinearity is not a problem. Winsorisation at 1st/99th percentile for outcomes and controls is standard. The complete-case sample (1,533 of 1,546) loses a trivial number of observations.

Two weaknesses to acknowledge in writing rather than hide:

- **Adjusted R² is negative for the long-run main spec** (−0.022 with controls; −0.076 baseline). That is a function of two-way FE eating most of the degrees of freedom relative to the variance explained. The within-R² (which `plm` does not print here, but can be reported) is more informative for FE models. Either compute it and report it, or note that two-way FE inflate the FE penalty and that statistical significance, not R², is the relevant criterion for the within-firm estimates.
- **The semi-only sub-sample (N=95) is too small to take seriously.** The headline coefficient (LM uncertainty −26.65***) is implausibly large and the sign of AI core flips. Do not present this as a "finding" — present it as a sensitivity that demonstrates the result is *not* mechanically driven by the six AI-as-core-operations semiconductor firms. The relevant comparison is column (3), the ex-semi sample, where the long-run AI core coefficient is 0.0064*** (vs. 0.0081*** in the full sample). That is the robust result.

## 5. Hypotheses vs. results

Your proposal stated:

- **H1.** Greater AI disclosure → positive *short-run* abnormal returns (attention/valuation channel).
- **H2.** Long-run *reversal* — those short-run gains unwind as substantive operating performance fails to materialise (the AI-washing channel).

What the data say (main spec, total segment, two-way FE, with controls, N=1,533):

| Channel | Short-run CAR [−1,+1] | Long-run AR (call-to-next-call) |
|---|---|---|
| AI core / 1,000 words | **0.0011 (n.s.)** | **0.0081\*\*\*** |
| AI adjacent / 1,000 words | −0.0009 (n.s.) | 0.0078 (n.s.) |
| LM tone | **0.0654\*\*\*** | 0.0210 (n.s.) |
| LM uncertainty | −1.18 (n.s.) | −2.16 (n.s.) |

H1 is **not supported.** The short-run mover is *tone*, not *AI content*. A one-unit increase in net positive tone (a large move on this scale) is associated with a 6.5 percentage-point announcement-window CAR, robust across specifications.

H2 is **rejected — and the data point in the opposite direction.** AI core mentions are *positively* associated with long-run abnormal returns, with the strongest loading in the Q&A segment (0.0079*** in tbl\_main\_long.txt's Q&A column from your prior analysis). If anything, this is consistent with substantive AI disclosure being *underpriced* at the announcement and the value being recovered over the subsequent quarter.

This is a real finding, not a null result. It also has a clean economic interpretation: scripted prepared remarks reflect tone management and get priced quickly via the tone channel; off-the-cuff Q&A revelations of substantive AI work are harder for analysts to digest immediately and get incorporated more slowly. Reframe the thesis around this. The literature on the slow-information-diffusion / PEAD-style underreaction provides the natural citation backbone.

## 6. Inconsistencies to fix in the draft *before* writing further

These are mechanical fixes. None require new analysis.

1. **CAR window label.** Replace every "CAR [−1,+3]" in `results discussion.docx` and `summary_stats.txt` with "CAR [−1,+1]" (3 trading days). The variable is `car_m1_p1`; the implementation is unambiguous.
2. **Stale numbers in `results discussion.docx`.** The draft quotes 0.0049** for the long-run AI core coefficient. The current output is 0.0081***. Replace with the current numbers from `tbl_main_long.txt` and `tbl_main_short.txt`. Do a full pass — anywhere a coefficient appears in the draft, sanity-check it against the current `output/regression/txt/` files.
3. **Sample period.** The proposal says 2022–2024. The data go through Q1 2026. State the actual realised window in the methods section ("Q4 2021–Q1 2026") and add a sentence explaining the extension (data availability, longer post-period to observe the full long-run window).
4. **Semi-firm exclusion.** The proposal said the baseline should *exclude* AI-as-core-operations semiconductor firms. The implementation keeps the full sample as baseline and treats ex-semi as a sensitivity. This is actually the *better* choice — excluding firms with the highest AI content would have biased toward a null. Defend the change explicitly: "Rather than ex-ante excluding firms in AI-intensive sectors, we retain the full S&P 100 and report ex-semiconductor robustness in the appendix; the magnitude of the long-run AI core coefficient is essentially unchanged (0.0081 → 0.0064), confirming the result is not mechanically driven by the six semiconductor firms."
5. **H1/H2 reframing.** Rewrite the hypotheses section. H1 should target *tone*; H2 should be reformulated as a *delayed-incorporation* hypothesis on AI substance, with the original AI-washing hypothesis discussed as the rejected alternative. The draft already gestures in this direction — finish the rewrite.

## 7. What you can defensibly conclude

State plainly, with the right hedges:

- AI-related disclosure on earnings calls is associated with positive long-run abnormal returns over the subsequent quarter, particularly when the AI content appears in the Q&A segment, in a sample of 98 S&P 100 firms over 2021Q4–2026Q1.
- The short-run announcement reaction is driven by tone of management language, not by AI content per se.
- The pattern is inconsistent with an AI-washing / reversal narrative in this large-cap sample. It is consistent with delayed incorporation of substantive AI disclosure into prices.
- Results are robust to firm and calendar-quarter fixed effects, firm-clustered HC1 standard errors, exclusion of AI-intensive semiconductor firms, and inclusion of standard earnings-announcement controls (SUE, analyst coverage, ROA, book-to-market, size, leverage).
- The estimates capture associations within firm and within calendar quarter; they do not establish a causal effect of AI disclosure on returns. Disentangling a causal channel would require an exogenous shock to AI disclosure, which is outside the scope of this study.

## 8. What you should *not* claim

- Do not claim the AI-washing hypothesis is supported. It is not.
- Do not claim a causal effect of AI mentions on returns.
- Do not present the semi-only column (N=95) coefficients as findings. Present them only to show that excluding the six firms does not overturn the main result.
- Do not generalise beyond the S&P 100. The mechanism could differ in small-cap, non-US, or private-firm contexts.
- Do not ignore the negative adjusted R² in the long-run model — address it (within-R², or a methodological note on two-way FE).

## 9. Suggested final checklist before writing

- [ ] Recompute and report within-R² for the FE models, or add a sentence on adjusted-R² interpretation under two-way FE.
- [ ] Add one sentence to methods acknowledging long-run AR is computed as a sum of daily ARs, not a BHAR, and cite the relevant trade-off.
- [ ] Standardise the CAR window label across draft, descriptive tables, and final paper.
- [ ] Replace all stale coefficients in the working memo with current `tbl_main_*.txt` numbers.
- [ ] Rewrite hypotheses to match what the data actually show.
- [ ] Add one paragraph in the limitations section on (a) endogeneity / association-not-causation, (b) dictionary-based measurement, (c) S&P 100 generalisability.

---

The work is solid. Write it up.
