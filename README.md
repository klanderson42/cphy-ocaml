# Computational Physics

Materials for UT Austin's graduate computational physics course, taught in Fall 2022 by William Gilpin

If you are enrolled in the course at UT, the syllabus and calendar is [here](https://docs.google.com/document/d/1URJmdpTVG8E2bLLu5xAHctICb6krbZ0fC0hO2i2xEXY/edit?usp=sharing)

# Requirements

We will use Python 3 with the following packages

+ numpy
+ matplotlib
+ scipy
+ scikit-learn

For your projects and other parts of the class, you might also need

+ scikit-image
+ umap-learn
+ statsmodels
+ pytorch
+ jax
+ numba


# Fixes, typos, and attribution

If you find any errors or typos, please open an issue or submit a correction as a pull request.

If you are using any of these materials for your own class, please submit typos or improvements as Issues or Pull Requests, so that we can improve this version of the course. Please let us know if you find these materials helpful, so that we can keep track of the course's impact, and potentially may help us get resources to support the course in future years.

If you'd like to reach out privately, please reach out at [wgilpin@[saxetu spelled backwards].edu](mailto:wgilpin@[saxetu spelled backwards].edu)

# Contents

+ [`hw1`](hw/hw1/hw1.ipynb)
+ [`hw2`](hw/hw2/hw2.ipynb)
+ [`hw3`](hw/hw3/hw3.ipynb)
+ [`hw4`](hw/hw3/hw4.ipynb)

+ [`lab1`](labs/lab1/lab1.ipynb)x

## Random Forest homework


## Some fun facts

An isolation forest uses a CART-like algorithm to identify outliers in a sample. A random forest is trained on data with outliers in order to isolate each point (as if it had its own label, different from all other points). Across many trees outliers, will tend to separate out at lower depths than non-outliers, allowing an outlier score calculation for each input datapoint, based on the average depth at which that datapoint splits from the remainder of the data:
+ https://cs.nju.edu.cn/zhouzh/zhouzh.files/publication/icdm08b.pdf
+ The normalizing factor in Eq. 2 has an interesting relationship with Big-O scaling of worst case vs average case for binary trees