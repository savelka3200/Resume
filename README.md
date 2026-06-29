# SKELAR IT — Analytics Intensive · Analytical Case Studies

Practical analytics projects built during the SKELAR IT Analytics Intensive.
Focus: SQL, A/B testing, and dashboard design on realistic product and marketing data.

**Stack:** SQL (BigQuery) · Python (pandas, scipy, statsmodels) · Tableau

---

## 1. CAC Channel Analysis — SQL / BigQuery

[📄 Full case study (PDF)](./CAC%20Channel%20Analysis.pdf)

Customer Acquisition Cost analysis across three paid channels (META, TikTok, Google)
using cumulative snapshot data from advertising dashboards.

- Handled cumulative data structure to avoid 2–3x inflated spend figures
- Deduplicated with `ROW_NUMBER()` window function (8,814 → 1,950 rows)
- Calculated real daily increments using `LAG()` instead of naive `SUM()`
- Built monthly CAC and LTV/CAC breakdown per channel

**Findings:** META has the lowest CAC ($3.10) and best LTV/CAC (2.0x);
Google is currently unprofitable (LTV/CAC = 0.88); TikTok has the weakest
post-click conversion. Also flagged the dataset as likely synthetic, since
CAC stayed stable to the cent across six months.

---

## 2. A/B Test — Credit Top-Up Reminder — Python

[📄 Full analysis (PDF)](./AB%20Test%20Credit%20Top-Up%20Reminder.pdf)

Evaluated whether a top-up reminder shown before in-app credits run out
increases conversion to first payment. ~125K users in the test dataset.

- One-sided z-test for proportions (α = 0.05, power = 0.80, 50/50 split)
- Cleaned the cohort by excluding contaminated control users
- Detected a reminder display instability in weeks 20.03–02.04 and traced
  it to implementation, not traffic quality

**Result:** p-value = 0.0111, statistically significant uplift.
Among users who actually saw the reminder, CR = 16.56% vs 2.99% for those
who didn't. Recommended fixing display logs before full rollout.

---

## 3. Marketing Performance Dashboard — Tableau

[📄 Description (PDF)](./Marketing%20Performance%20Dashboard.pdf) ·
[🔗 Live dashboard on Tableau Public](https://public.tableau.com/app/profile/anastasiia.savelieva6332/viz/MarketingPerformanceDashboard_17817168294800/Dashboard)

A monthly marketing performance dashboard designed so the team can read the
full picture in about two minutes. Blocks grouped by question type:
"what's happening now," "why," and "what to do next."

- Eight KPI tiles with 7d and 90d revenue horizons
- 7d / 90d ROAS parameter toggle across all charts
- Revenue vs Spend combined chart, ROAS by channel against a break-even line
- Geo × channel heatmap, budget quadrant scatter, channel spend treemap

**Insight:** Meta and TikTok together take 71% of the budget while both run
below break-even, making channel reallocation the central recommendation.
