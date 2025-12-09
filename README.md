# Strategic HR Retention Analysis: Reducing Employee Attrition

Author:Asin Sahana Fathima  
Date: 10.11.2025  
Tools: R (dplyr, ggplot2, caret)

## 1. Executive Summary
The organization faces a challenge where valuable talent is leaving. Our analysis reveals that attrition is not random—it is primarily driven by Burnout (Overtime), Income Disparities, and Lack of Career Progression. By targeting at-risk employees (specifically Lab Technicians and Sales Reps), the company can predict and prevent up to 85% of turnover events.

## 2. Business Problem
High employee turnover is resulting in increased recruitment costs, lost productivity, and lower morale.
Objective: Use historical employee data (IBM HR Analytics dataset, n=1,470) to identify who is leaving and why.

## 3. Key Findings & Insights

### A. The "Burnout" Effect
Overtime is the single strongest predictor of attrition.
* Employees working overtime have a 30%+ attrition rate, compared to 10% for those who do not.
* Working overtime makes an employee 3.7x more likely to quit.

### B. The Income Gap
There is a significant gap in median monthly income between leavers ($3,202) and stayers ($5,204). Attrition is heavily concentrated in brackets earning below $3,000/month.

### C. The "Loyalty" Danger Zone
The Tenure Density Plot shows a sharp spike in attrition between 0 and 2 years. If an employee stays past the 3-year mark, they are highly likely to remain loyal.

### D. Role-Specific Risks
High Risk: Sales Representatives and Laboratory Technicians.
Insight: These roles are the lowest paid and most likely to work overtime.

## 4. Predictive Modeling
We developed a Logistic Regression Model to predict attrition risk with ~85% accuracy.
Top 5 Risk Factors:
1.  OverTime (Yes)
2.  Job Role (Sales Rep)
3.  Laboratory Technician Role
4.  Business Travel (Frequent)
5.  Distance From Home

## 5. Strategic Recommendations
1.  Implement an Overtime Cap:Review workload in Sales and R&D; limit mandatory overtime.
2.  First 2 Years" Retention Program:Launch mentorship and conduct "Stay Interviews" at 6-month marks.
3.  Compensation Adjustment:Conduct a market rate review for Lab Technicians to address the income gap.

---
*View the full PDF report in the files above for detailed visualizations.*
