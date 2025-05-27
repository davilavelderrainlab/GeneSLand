GeneSLand (Gene Specificity Landscape) is an R package for comparative 
transcriptomic analysis following the framework described in [Bot and Davila-Velderrain, 2025].

# Analysis tools

GeneSLand contains functions to:

- Create a Specificity Single Cell Experiment Object (`SSCE`)
- Calculate the Specificity scores (`AUC`, `dRate`, `lbSpec`)
- Plot the Specificity landscape
- Plot the expression Breadth-expression Level (LB) lines
- Map new genes/gene groups on an existing SSCE
- All the SSCE objects described in the [article]

In the [vignette], we provide an example on how to build and visualize a SSCE.

# Basic installation

To install `GeneSLand` from GitHub:

```{r}
install.packages("devtools")

devtools::install_github("https://github.com/davilavelderrainlab/GeneSLand")
```

# Bug report

Please use the [issues] to submit bug reports.

# Reference

If you use `GeneSLand` in your work, please cite

> **Gene specificity landscapes for comparative transcriptomic analysis**
>
> Erik Bot & José Davila-Velderrain
>
> _Journal_ Date. doi: [doi](https://github.com/davilavelderrainlab/GeneSLand).

[Bot and Davila-Velderrain, 2025]: https://github.com/davilavelderrainlab/GeneSLand
[article]: https://github.com/davilavelderrainlab/GeneSLand
[vignette]: https://github.com/davilavelderrainlab/GeneSLand/blob/main/vignettes/GeneSLand.Rmd
[issues]: https://github.com/davilavelderrainlab/GeneSLand/issues
