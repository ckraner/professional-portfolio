# Application Screenshots

## Overview

This folder contains visual snapshots of the Shiny applications used for variable labeling, data exploration, and statistical modeling. The images are provided to show the interface layout, available controls, and the workflow across different tabs in the app. Each section below corresponds to one of the major interface areas.

---

## Screenshots



- 01-data-rangler.png - This is an image of label.explor.r, the companion app to the stats.explor.r dashboard. It handles variable selection and manipulation. The image shows a simple table for each variable, the ability to make specified values NA, highlighting what can be made into factors, and creating ordered factors. Once complete, it creates a structured database that stats.eplor.r can handle.

- 02-univariate-explore.png - This image is the landing page in the stats.explor.r interface. It shows basic statistics for each variable, as well as more advanced statistics below it. Basic plots are included, and variable selection can be done in the top panel.

- 03-multivariate-explore.png - This image shows general statitics for all variables in the database. It includes the basic psych::describe table with skew and curtosis, as well as pearson correlations and mahalanobis distances when the dataset allows.

- 04-ancova.png - This image shows the ANOVA and ANCOVA interface. It allows dynamic equation building and smart switching between ANOVA and ANCOVA. In addition, it pulls together several packakges in order to provide advanced fit analyses and tables of coefficients. When running multiple models, it gives comparison of fit measures between the two models.

- 05-mancova.png -  This image shows the MANOVA and MANCOVA interface. I show this because it was the real reason I built this interface. Everything else carried from previous projects I had done. In it, you see you can use multiple dependent variables along with a regression table and factor contrasts. The tools to do these pieces were so disconnected and vast that you had to carry multiple dataframes, each setup a little differently for each test. And then supplement the results by pulling values yourself into other brittle functions and packages.
