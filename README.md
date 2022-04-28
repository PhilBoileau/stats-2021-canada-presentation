# Cross-validated Loss-based Covariance Matrix Estimator Selection in High Dimensions

Slides presented at Statistics 2021 Canada. In collaboration with Nima S. Hejazi, Mark J. van der Laan and Sandrine Dudoit.

### Abstract

The covariance matrix plays a fundamental role in many modern exploratory and inferential
statistical procedures, including dimensionality reduction, hypothesis testing, and regression. In
low dimensional regimes, where the number of observations far exceeds the number of variables,
the optimality of the sample covariance matrix as an estimator of this parameter is well-established.
High-dimensional regimes do not admit such a convenience, however. As such, a variety of estimators
have been derived to overcome the shortcomings of the sample covariance matrix in these settings.
Yet, the question of selecting an optimal estimator from among the plethora available remains
largely unaddressed. Using the framework of cross-validated loss-based estimation, we develop the
theoretical underpinnings of just such an estimator selection procedure. In particular, we propose a
general class of loss functions for covariance matrix estimation and establish finite-sample risk bounds
and conditions for the asymptotic optimality of the cross-validated estimator selector with respect
to these loss functions. We evaluate our proposed approach via a comprehensive set of simulation
experiments and demonstrate its practical benefits by application in the exploratory analysis of two
single-cell transcriptome sequencing datasets. A free and open-source software implementation of
the proposed methodology, the `cvCovEst` R package, is briefly introduced.
