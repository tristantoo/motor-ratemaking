# Motor TPL ratemaking on freMTPL2

A pricing analyst's handover package on 668,874 French motor third party liability policies: frequency and severity GLMs, rating relativities with credibility and capping, a loss ratio rate indication, lift and double lift charts, a gradient boosting challenger tested with a paired bootstrap, and a mock rate filing PDF.

## Headline results (test set, 133,775 policies)

| Comparison | Poisson deviance reduction | 95% paired bootstrap CI |
|---|---|---|
| Proposed GLM vs exposure only | 5.09% | 4.56% to 5.60% |
| Proposed GLM (8 variables) vs current GLM (3 variables) | 0.55% | 0.35% to 0.73% |
| GBM vs proposed GLM | 1.68% | 1.38% to 2.00% |
| GLM plus DrivAge x BonusMalus interaction vs proposed GLM | 0.35% | 0.24% to 0.48% |
| GBM vs GLM with that interaction | 1.34% | 1.03% to 1.65% |
| Severity Gamma GLM vs flat mean (Gamma deviance) | -0.03% | -0.87% to 0.85% |

Frequency Gini: current GLM 0.281, proposed GLM 0.302, GBM 0.335. Pure premium Gini: current plan 0.294, proposed 0.326.

Plain reading:

1. **The GBM beats the GLM, and the gap is real.** Its interval excludes 0, and the GLM vs GBM double lift shows the GLM's actual to predicted ratio running from 0.61 to 1.70 across deciles while the GBM's stays between 0.95 and 1.05. The GLM misses structure. One interaction chosen in advance (young drivers carry high bonus malus) closes about a fifth of the gap (0.35 of 1.68 points). A filing would still use a GLM for transparency, so the next step is finding the remaining interactions, not swapping in the GBM.
2. **The severity model adds nothing.** The Gamma GLM is no better than a flat mean on the holdout, so the proposed plan uses frequency relativities with a flat severity. That decision was made on the test set and is disclosed in the filing.
3. **The indication (+5.85%) is driven by assumptions.** Trend, expenses, and the current premium are illustrative (see below). The exhibit shows the mechanics, not a finding about French motor rates.
4. **Dislocation is large:** 55.7% of policies move more than 10% either way under the revenue neutral structure, because the current plan has 3 rating variables and the proposed plan has 8. A real filing would phase this in.

What would have falsified the GBM claim: a bootstrap interval for "GBM vs proposed GLM" that included 0, or a double lift where the GLM's ratio was as flat as the GBM's. Neither happened.

## Data

- Frequency: OpenML 41214, freMTPL2freq, 678,013 policies ([OpenML](https://www.openml.org/d/41214)). Severity: OpenML 41215, freMTPL2sev, 26,639 claim amounts ([OpenML](https://www.openml.org/d/41215)). License CC0.
- **Severity gap:** policies with `IDpol <= 24500` carry 9,675 claims in the frequency file but only 302 severity records. All 9,139 of those policies are dropped so frequency and severity describe the same claims.
- **Counts from the severity file:** on the remaining policies, 266 claim counts disagree with the number of severity records. Claim counts are taken from the severity file.
- Exposure is capped at 1 year.
- **Large losses:** each claim is capped at 35,651, the 99.5th percentile of training claims. Capping removes a lot: the excess factor that spreads the removed amount back over all policies is 1.36.

## Split and protocol

Random 80/20 split stratified by claim count (0, 1, 2+), seed 2026: 535,099 train, 133,775 test. **This is not a forward chaining test.** The data have no policy dates, so a model trained on earlier business and tested on later business is impossible here. Results can be optimistic relative to deployment.

The GBM's early stopping uses a 10% slice of train (seed 2027), never the test set. Every comparison above is a paired bootstrap over test policies (1,000 draws) of the percent reduction in total deviance.

Overdispersion check: Pearson dispersion of the Poisson GLM is 1.82, and a negative binomial fits better by AIC (168,959 vs 169,372). But no NB coefficient moves a relativity by more than 1.4%, so the Poisson mean model is kept.

## What is estimated vs assumed

| Item | Status |
|---|---|
| Frequency and severity GLMs, relativities, Z values, large loss cap, excess factor | estimated from training data |
| Loss development factor 1.00 | assumed: data treated as ultimate |
| Trend 3% a year for 2 years, ULAE 10%, variable expense 25%, profit 5% | illustrative |
| Current plan | defined here: Poisson GLM on BonusMalus, DrivAge, and Region with a flat severity, priced at experience period cost with no trend |
| Credibility | limited fluctuation, full standard 1,082 claims; Z = min(1, sqrt(claims / 1082)) |
| Caps | each level's change limited to +/- 15% (8 of 68 cells capped; see `outputs/capped_cells.csv`) |

## Run it

```
Rscript -e "install.packages(c('data.table','xgboost','testthat','jsonlite'))"
Rscript R/download.R          # about 1 minute; writes data/raw/*.rds
Rscript run.R                 # about 5 minutes; writes outputs/
Rscript -e "testthat::test_dir('tests/testthat')"
```

Tested with R 4.6.1, data.table 1.18.6.1, xgboost 3.2.1.1.

## Files

- `R/download.R`: fetches both ARFF files from OpenML and checks MD5.
- `R/rating.R`: banding, relativities, one way, credibility, caps, indication, lift, double lift, Gini, deviance, paired bootstrap.
- `R/exhibits.R`: charts and the mock filing PDF.
- `run.R`: the whole pipeline.
- `tests/testthat/test_rating.R`: base level relativity is 1, lift bins sum to total exposure, indication matches a hand computed case, credibility and caps, Gini sanity.
- `outputs/`
  - Metrics: `metrics.json`.
  - Tables: `relativities.csv`, `capped_cells.csv`, `assumptions.csv`, `holdout_bootstrap.csv`, `gini.csv`, lift and double lift CSVs, `dislocation.csv`, GLM coefficient tables.
  - Charts: `oneway_vs_glm.png`, `lift.png`, `double_lift.png`, `lorenz.png`, `dislocation.png`.
  - Filing: `mock_filing.pdf`, 10 pages, each footed "MOCK FILING. Educational exercise on public data. Not an actual rate filing."

The one way vs GLM chart shows why the GLM matters. Drivers aged 18 to 20 have 3.10 times the base claim frequency one way, but only 1.14 times after the GLM adjusts for their bonus malus levels.

## Authorship

Built with AI coding assistance (Claude Code). I set the scope and questions and reviewed the methods, code and results.
