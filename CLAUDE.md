# STA510 — Interactive Study App

## Project
Quarto + WebR study reference and project tracker for STA510 Bayesian Methods (Göteborg University, autumn 2026). Built from the STA520 app template. Content follows the course notes *Bayesian Statistical Methods* (Koen Simons, 2nd ed., Aug 2026: `../BAYnotes20260829_clean.pdf`) and the syllabus.

- **Source**: `C:/Users/nepet/Documents/Studier/Masterprogram i tillämpad biostatistik/STA510 Bayesian Methods/App/`
- **Stack**: Quarto website, knitr engine (every module page sets `engine: knitr`), WebR for `{webr-r}` cells (base R only, no extra packages), plain JS includes.
- **No written exam**: the course is examined by seminars (4 of 5) and an individual project (GLMM on self-chosen data). The app therefore has no exam flags or exam drills.

## Pages
- `module1.qmd` … `module6.qmd`: one page per course-notes module. Structure: intro · sources callout · "Before you start" callout · **Part A · Concepts** (topic sections with quizzes, misconception callouts) · When to use · Limitations · **Part B · Practice** (WebR browser exercises, then RStudio brms tasks with hidden solutions) · Why-questions · Spaced review
- `project.qmd`: ESS feasibility + ten project steps (`.step` topics with Not started / In progress / Done)
- `dashboard.qmd`, `glossary.qmd`, `about.qmd`, `index.qmd`
- `tasks/*.R`: downloadable RStudio task scripts (published via `project: resources`). Every solution on the pages was run with these scripts; numbers quoted come from those runs.

## Topic sections (drive the checklist and dashboard)
Level-2 headings:

```markdown
## 3.9 Convergence II: R-hat {#t3-9 .topic flag="ilo project"}
## 2.4 Flat priors, improper priors and the classical interval {#t2-4 .topic}
## Step 4 · Priors and a prior predictive check {#p4 .topic .step flag="project"}
```

- `flag` is a space-separated list: `ilo` = covers a syllabus intended learning outcome (official outcomes, mapping made for the app); `project` = AI-inferred relevance for the project.
- `build_topics.py` (pre-render) writes `_topics-data.html` (`window.STA510_TOPICS`). Do not edit that file by hand.
- IDs must stay stable: status is stored per ID in localStorage key `sta510_topic_status`.

## Includes (after body)
- `_topics-data.html`, `_topics.html` (status buttons, badges, sidebar counters, dashboard)
- `_quiz.html` + `_quiz-filter.lua`: quizzes (`:::: {.quiz}`, bold = correct)
- RStudio tasks: `::: {.rstudio-task}` whose first paragraph is the task title, with a collapsed `callout-tip` "Solution"

## Content rules
- English; define abbreviations on first use per page.
- **Own examples only.** Never reproduce the course notes' examples, scripts or datasets (taxicab, bit flips, school meals, infant mortality, magnesium) or solve the notes' exercises. The site is public.
- **No ESS data** in the repo or on the site; the project starter script reads the user's local file.
- Quiz options roughly equal length; the correct one must not stand out.
- Any number stated in a solution must come from an actual run (rerun `tasks/*.R` after changing a task).
- R code: explicit, readable variable names; no compact metaprogramming.

## Render and deploy
```powershell
& "C:\Program Files\Quarto\bin\quarto.exe" render
```
Then git add/commit/push `master` and `quarto publish gh-pages --no-render --no-prompt`. Stop any local web server serving `_site` first (it locks the folder on Windows).
