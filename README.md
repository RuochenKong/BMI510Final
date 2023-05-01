# BMI 510 Final Project
Implementations of 20 functions could be used for statistical analysis.

### Fucntion Descriptions
1. `rando(x,n=1,replace=T)`  Perform random sampling on the input x.
2. `is_min(x,na.rm=T)` Check whether each element in x is the minimum of it.
3. `is_max(x,na.rm=T)` Check whether each element in x is the maximum of it.
4. `rep_mat(x, M=1, N=1)` Repeat the input data by rows and/or by columns.
5. `classes(x)` Find the classes of each variable in the input x.
6. `df_scale(x, center = T, scale = T)` Scale the numeric columns of the input x.
7. `log_likelihood_norm(x, mean, sd)` Calculate the log-likelihood of x under the normal distribution.
8. `log_likelihood_unif(x, min, max)` Calculate the log-likelihood of x under the uniform distribution.
9. `log_likelihood_chisq(x, df)` Calculate the log-likelihood of x under the chi-square distribution.
10. `log_likelihood_f(x, df1, df2)` Calculate the log-likelihood of x under the f distribution.
11. `log_likelihood_t(x, df)` Calculate the log-likelihood of x under the t distribution.
12. `sensitivity(pred,truth)` Calculate the sensitivity of the prediction.
13. `specificity(pred,truth)` Calculate the specificity of the prediction.
14. `precision(pred,truth)` Calculate the precision of the prediction.
15. `recall(pred,truth)` Calculate the recall of the prediction.
16. `accuracy(pred,truth)` Calculate the accuracy of the prediction.
17. `f1(pred,truth)` Calculate the f1 score of the prediction.
18. `minimum_n_per_group = function(d,power = 0.8)` Calculate the minimum n needed for a two-sample t-test.
19. `r2(pred,truth)` Calculate the R-squared statistics.
20. `adj_r2(pred,truth)` Calculate the adjusted R-squared statistics.

### Installation
<pre>
git clone https://github.com/RuochenKong/BMI510Final.git
</pre>
