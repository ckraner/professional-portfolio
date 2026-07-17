# Project Artifact - Shiny Multivariate Analysis Tool

### A workflow I built during a 700‑level multivariate statistics course

I built this Shiny application during a 700‑level multivariate statistics course because the workflow in R was too scattered to support the analysis we were doing. The datasets were assigned, but everything else was open‑ended, and R made even basic preparation feel disjointed. Cleaning the data, reshaping it, recoding variables, checking assumptions, running models, and comparing fit all lived in different places. Nothing lined up cleanly, and every assignment meant repeating the same messy sequence of steps.

To fix that, I built two tools: a data‑wrangling companion app and the main multivariate analysis dashboard. The wrangler came first. The course datasets often used Y/N variables, and R didn’t treat them consistently. Sometimes they came through as characters, sometimes as factors, and sometimes as logicals. That inconsistency caused problems in multivariate models, so the wrangler converted them into numeric form when needed, created proper factors when categorical encoding made more sense, and added simple verification steps to make sure variables were interpreted the same way across packages. It also handled reshaping, variable selection, and basic distribution checks. The goal was simple: prepare the data once, prepare it correctly, and avoid repeating the same fixes for every assignment.

Once the data was stable, the main app handled the modeling. It supported the core studies we used throughout the course — ANOVA, ANCOVA, GLM, MLM, MANOVA, MANCOVA — along with the diagnostics and assumption checks that go with them. I could build a model, check assumptions, look at appropriate fit measures, and compare goodness‑of‑fit between models without jumping between libraries or making long blocks of repetitive code. It made the analysis feel like a single process instead of a set of disconnected tasks. The tool didn’t replace the statistical thinking, but it removed a lot of friction and let me focus on the actual work.

Because Shiny was still early in its development, I had to work around several limitations. There were no modules, no dynamic UI generation, and no real support for multi‑page layouts. To make the apps usable, I built my own routing system, created reactive bindings manually, and generated UI code by writing R files on the fly. It wasn’t elegant, but it worked, and it let me build the workflow I needed even though the framework didn’t yet support it. That experience taught me how reactive systems behave under the hood and how to improvise when a tool doesn’t offer the features you expect. It shaped how I think about frameworks today — if something is missing, I’m comfortable building the piece myself.

At the time, Shiny didn’t have great packaging and distribution. That’s why I wrote an installer package to go along with the main apps. It set up the directories and created the files automatically so classmates could install and run the tools without touching the internals. A few people shared it with others, and the workflow ended up circulating informally through the class. It wasn’t official, but it became useful because it solved a real problem: the course required repeated, complex analyses, and this setup made those analyses easier to manage and easier to verify.

Looking back, these projects weren’t really about Shiny. They were about creating a workflow that made sense in a course where the tools didn’t naturally fit together. They taught me how to unify scattered steps, how to build structure around statistical methods, and how to create checks that make analysis more trustworthy — from the modeling all the way back to the data wrangling that made everything possible.

---

## AI Collaboration Disclosure

Parts of this project’s documentation were drafted with the assistance of Microsoft Copilot. All decisions about content, structure, and accuracy were my own, and the tools themselves were designed, written, and implemented without AI involvement.
