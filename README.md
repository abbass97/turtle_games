# turtle_games
# Turtle Games Customer Analytics Report

##  Project Overview:

This project was developed as part of the **LSE Career Accelerator in Data Analytics** during the course **Advanced Analytics for Organisational Impact**. The goal was to analyse 'Turtle Games’ customer data to derive actionable business insights through descriptive statistics, predictive modelling, clustering, and sentiment analysis.

---

## Business Objective:

Turtle Games aims to:
- Understand how customers accumulate loyalty points.
- Segment customers for targeted marketing.
- Predict loyalty using demographic and behavioural data.
- Analyse customers' reviews sentiments to improve customer experience.


---

## Analytical Approach:

### 1. **Data Wrangling**
- Source: `turtle.csv` (cleaned to `clean_reviews.csv`)
- Removed irrelevant columns, renamed for clarity (e.g., `renumeration` → `salary`)
- Final dataset: 9 columns, 2000 rows

### 2. **Exploratory and Statistical Analysis**
- Histograms, boxplots, scatterplots, and correlation matrices were used.
- Key findings:
  - Loyalty is skewed, with a few high-earning users.
  - Spending and salary are most strongly correlated with loyalty.

---

## Predictive Modelling:

### **Regression Analysis**
- Tested 7 models; **Model 2 (Salary + Age + Spending)** was most accurate.
- Achieved: RMSE = 513.31, MAE = 394.98
- Found heteroscedasticity, but model remains interpretable and strong.

### **Decision Tree Regressor**
- Best model: **Spending + Salary + Age** (R² = 0.9961, MAE = 26)
- Pruning improved simplicity but with slightly reduced accuracy.

---

## Customer Segmentation:

### **K-Means Clustering**
- Optimal clusters: 5 (validated using Elbow and Silhouette methods)
- Segment insights:
  - Cluster 2: High salary & spending – ideal for premium offers
  - Cluster 1: High salary, low spending – target with incentives
  - Cluster 4: Low income & spending – sensitive to pricing

---

## Sentiment Analysis:

- Used `nltk` and `TextBlob` on customer reviews
- Most sentiments were **neutral to slightly positive**
- Common positive terms: *fun, game, play, great, love*
- Few negative reviews pointed to usability, product quality and instructions issues

---

## Recommendations

- **Boost Loyalty**:
  - Target high-income/low-spending customers with promotions
  - Reward loyal, high-spending clusters with exclusive offers

- **Enhance Data Collection**:
  - Redesign review forms with structured prompts
  - Capture **purchase frequency**, not just product codes

- **Improve Product Experience**:
  - Promote best-performing products
  - Use negative feedback to address usability concerns

- **Refine Customer Strategy**:
  - Focus campaigns on age group 32–34
  - Tailor marketing based on spending/salary clusters

---


## Author

**Saima Abbas**  
_30th September 2024_

---
