# A Machine Learning Analysis of Factors Leading to Major League Baseball Postseason Berths

**Bachelor of Science in Mathematics (Statistics) Honors Thesis**  
**East Tennessee State University**  
**Completed: May 2025**

**Chase Foster**

---

## Overview

This thesis investigates which factors contribute most to Major League Baseball (MLB) teams qualifying for the postseason. Using historical team statistics from the 2005–2024 seasons, I applied a variety of supervised and unsupervised machine learning techniques to identify the variables most strongly associated with postseason qualification and to evaluate the predictive performance of multiple classification models.

The project demonstrates an end-to-end machine learning workflow, including data preprocessing, dimensionality reduction, clustering, classification, model evaluation, and interpretation.

---

## Objectives

The primary goals of this project were to:

- Identify the most important team performance metrics associated with postseason qualification.
- Compare the predictive performance of multiple machine learning algorithms.
- Evaluate both supervised and unsupervised learning methods.
- Develop a model capable of predicting MLB postseason teams using historical statistics.

---

## Dataset

- **Source:** Stathead Baseball (Sports Reference)
- **Time Period:** 2005–2024 MLB Seasons
- **Observations:** 810 team seasons
- **Predictor Variables:** 57 offensive and defensive team statistics

The dataset includes batting, pitching, and fielding metrics used to model postseason qualification.

---

## Machine Learning Methods

### Unsupervised Learning

- Principal Component Analysis (PCA)
- Hierarchical Clustering
- K-Means Clustering

### Supervised Learning

- Logistic Regression
- K-Nearest Neighbors (KNN)
- Linear Discriminant Analysis (LDA)
- Polynomial Regression
- Cubic Splines
- Natural Cubic Splines
- Generalized Additive Models (GAMs)
- Decision Trees
- Bagging
- Random Forests

Each model was evaluated using validation datasets and cross-validation where appropriate.

---

## Results

Several machine learning models produced strong predictive performance.

The strongest overall model was **Random Forest**, which provided excellent predictive accuracy while also identifying the most influential variables associated with postseason qualification.

Across multiple methods, the variables most consistently associated with postseason success included:

- ERA+
- OPS+
- Runs Allowed
- Runs Scored
- SO/BB Ratio

These findings suggest that successful teams balance both offensive production and strong pitching performance.

---

## Technologies Used

- R
- RStudio
- tidyverse
- randomForest
- MASS
- cluster
- mclust
- fossil
- splines
- ggplot2

---

## Repository Structure

```
code/
    R scripts used throughout the analysis

figures/
    Visualizations generated during the study

thesis/
    Final honors thesis

README.md

LICENSE
```

---

## Key Skills Demonstrated

- Machine Learning
- Statistical Modeling
- Classification
- Dimensionality Reduction
- Feature Selection
- Model Evaluation
- Cross Validation
- Data Visualization
- Exploratory Data Analysis
- Sports Analytics

---

## Future Improvements

Potential future work includes:

- Incorporating individual player-level statistics
- Testing modern gradient boosting algorithms (XGBoost, LightGBM)
- Evaluating additional feature engineering techniques
- Predicting playoff advancement beyond postseason qualification
- Developing an interactive dashboard to visualize model predictions

---

## Author

**Chase Foster**

Bachelor of Science in Mathematics (Statistics)

East Tennessee State University
