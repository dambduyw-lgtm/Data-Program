# Descriptive Statistics — Interpretation & Thesis Implications

---

## 1. Sample Overview

The final dataset covers **98 S&P 100 firms** across **1,547 earnings calls** from late 2021 through early 2026, roughly 16 calls per firm. The panel is near-perfectly balanced — 93 of 98 firms have exactly 16 observations — which is a methodological strength worth flagging explicitly in your thesis, as balanced panels produce more reliable within-estimator results.

---

## 2. AI Mention Intensity (Figures 1, 2, 6)

AI language is present but uneven. **55.7% of calls contain at least one core AI keyword**, rising to 60.9% with the broader adjacent dictionary. This means a meaningful share of the sample — roughly 44% of calls — records zero core AI mentions, which creates a heavily right-skewed distribution and is why the quintile binning in Figure 5 collapses to only three distinct groups for the core measure (many ties at zero).

The time trend (Figure 1) will almost certainly show a visible inflection around **Q1–Q2 2023**, coinciding with the public launch of ChatGPT and the surge of AI discourse in corporate communication. This is a narrative anchor your coach will likely want to see highlighted.

Figure 6 (presentation vs. Q&A split) is worth watching closely. If Q&A AI intensity rises faster than presentation intensity over time, it would suggest that analysts are increasingly *asking* about AI rather than firms scripting it in — a useful distinction for defending the authenticity of your signal.

Removing semiconductor firms (NVDA, AMD, INTC, QCOM, AVGO, TXN) reduces the mean core AI intensity from **1.35 to 1.07 per 1,000 words** — a 21% drop — confirming that these six firms meaningfully inflate the aggregate signal. The adjacent measure barely moves (0.352 → 0.332), which makes sense: adjacent AI terms are more diffuse and less concentrated in semiconductor-specific language.

---

## 3. Return Distributions (Figure 3)

Both return measures centre very close to zero, as expected for abnormal returns:

| Sample | CAR mean | CAR median | Long-run mean | Long-run median |
|---|---|---|---|---|
| Full | +0.34% | +0.08% | +0.41% | −0.02% |
| Ex-semiconductor | +0.35% | +0.13% | +0.19% | −0.06% |

Two things stand out. First, the **median long-run abnormal return is slightly negative** in both samples (−0.02% and −0.06%), while the mean is positive. This is driven by a small number of large positive outliers, particularly from high-AI semiconductor names. After excluding those firms the long-run mean drops from +0.41% to +0.19%, suggesting semiconductors pull the aggregate long-run performance upward noticeably. Second, the short-run CAR is positive on average — consistent with the literature finding a mild positive announcement effect — but the magnitudes are economically small and the distributions are wide, so statistical significance in regression will depend heavily on your controls.

---

## 4. AI Intensity vs. Returns — The Contradiction (Figure 5)

This is where the tension with your hypotheses becomes visible. The raw quintile patterns show:

**Core AI — full sample:**
- Q1 (zero/lowest mentions): CAR = +0.26%, long-run = −0.25%
- Q2 (moderate mentions): CAR = −0.30%, long-run = −0.78%
- Q3 (highest mentions): CAR = +1.22%, long-run = **+3.61%**

**The long-run pattern is the opposite of H2.** Firms in the highest AI intensity group earn *better* long-run abnormal returns, not worse. The expected AI-washing pattern — initial hype followed by reversal — is simply not visible in this raw data.

The ex-semiconductor sample tells the same story, just at smaller magnitudes (top-quintile long-run: +2.38% vs +3.61%), so semiconductor firms amplify the effect but do not create it.

**What explains this?** Two likely factors. First, the S&P 100 is a survivorship-biased sample of the most successful, institutionally covered large-caps. These firms have the resources and credibility to actually implement AI — meaning their AI mentions may reflect genuine strategic investment rather than empty narrative. Second, the sample window captures the early phase of AI adoption (2021–2026), before any market correction for AI hype has had time to materialise in long-run returns.

---

## 5. What This Means for Your Thesis

**The contradiction is not a crisis — it is a finding.** Several paths forward are worth discussing with your coach:

**Reframe H2.** Rather than predicting long-run *reversal*, you could reframe it as a test of whether AI mentions signal genuine capability (positive long-run) versus narrative excess (zero or negative long-run). The current result supports the former, at least for S&P 100 firms — which is defensible and interesting.

**Segment by AI intensity level, not quintile.** The zero-vs-nonzero boundary matters more than quintile rank. Firms with *any* core AI mentions may be categorically different from those with none. A binary treatment (AI mentioner vs non-mentioner) might yield cleaner results than the continuous intensity measure.

**Lean into the controls.** The raw quintile patterns conflate AI adoption with firm quality. A firm that mentions AI a lot is also likely growing, profitable, and well-covered by analysts — all of which predict positive returns independently. The panel regression with firm fixed effects and WRDS controls (once you pull them from Compustat) is exactly where these confounds get addressed. The raw descriptive contradiction may dissolve once you partial out firm-level heterogeneity.

**The ex-semiconductor comparison is a strength.** Even if the overall story changes once you add controls, showing that the pattern holds in both full and ex-semiconductor samples demonstrates robustness and rules out the most obvious confound. Keep both in the thesis.

---

## 6. Summary for Coach Meeting

| Item | Status |
|---|---|
| Data pipeline | ✅ Complete — 1,547 obs, 98 firms, 2021–2026 |
| Textual analysis | ✅ AI intensity + LM sentiment extracted and validated |
| Fiscal period NAs | ✅ Fixed — "full year" transcripts mapped to Q4 |
| Panel structure | ✅ Near-perfectly balanced (95% of firms: 16 calls) |
| Descriptive figures | ✅ 8 figures + 3 summary tables generated |
| Semiconductor exclusion | ✅ Scaffolded as sensitivity check |
| Panel regression | 🔄 Script ready — awaiting WRDS controls (mkt cap, ROA, leverage) |
| Time fixed effects | ⚠️ To add (`effect = "twoways"`) once controls are in place |
| Preliminary finding | ⚠️ Raw data contradicts H2 — long-run returns *positive* for high-AI firms |
