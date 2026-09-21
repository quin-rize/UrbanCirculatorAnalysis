# Legacy: 2023 undergraduate analysis

`Final Paper.docx` and `regression.R` are the original version of this
project, written for an undergraduate econometrics course in 2023. They're
kept here for the record, not as the current analysis -- see `/analysis`
for that.

The original paper tested whether the Delmar Loop Trolley raised home
values in nearby St. Louis zip codes, using a simple two-period
difference-in-differences (`lm(HomeValue ~ time + treated + did)`) run
separately for several candidate treatment dates. It found no
statistically significant effect under any plausible date, and flagged its
own one "significant" result (a 20-year window) as likely confounded.

What it couldn't do: check its own identifying assumption. Parallel
pre-trends were assessed by eye, against a line chart, rather than tested.
The current analysis revisits the same question with panel fixed effects,
clustered standard errors, and an explicit event-study test of parallel
trends -- which turns out to matter: it catches a real pre-trend
confound in the pooled three-city extension that this original approach
had no way to see.
