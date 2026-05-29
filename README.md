GeneSLand (Gene Specificity Landscape) is an R package for comparative 
transcriptomic analysis following the framework described in [Bot and Davila-Velderrain, 2025].

# Analysis tools
 
With `GeneSLand` you can:
 
- calculate and visualize gene expression level-breadth (L-B) behavior
- build and visualize gene specificity landscapes
- calculate gene specificity scores (`AUC`, `dRate`, `lbSpec`)
- map genes/gene groups to reference landscapes
- explore precomputed tissue, cell type, and neuronal specificity landscapes

In the [vignette], we provide an example on how to build and visualize a SSCE.

# Installation

To install `GeneSLand` from GitHub:

```{r}
install.packages("devtools")

devtools::install_github("https://github.com/davilavelderrainlab/GeneSLand")
```

Additionally, the `GeneSLand` package requires the use of a containerization platform 
(`Docker` or `Singularity`). You can choose one according to your computing environment.

## Docker 

Docker is an open source containerization platform suitable for general-purpose 
computing environments and compatible with macOS, Windows, and Linux operating systems. 

To install it, here are specific instructions for each platform: 
* macOS: https://docs.docker.com/desktop/setup/install/mac-install/
* Windows: https://docs.docker.com/desktop/setup/install/windows-install/
* Linux: https://docs.docker.com/desktop/setup/install/linux/

## Singularity 

Singularity is a containerization technology designed primarly for high-performance computing (HPC) 
environments and multi-user shared systems. It is compatible with Linux distributions commonly used for HPC systems.
It can be installed following the guide at: https://docs.sylabs.io/guides/3.0/user-guide/installation.html. 

In case singularity is already available via environment modules, 
no further installation or configuration is required.

# Bug report

Please use the [issues] to submit bug reports.

# Reference

If you use `GeneSLand` in your work, please cite

> **Gene specificity landscapes for comparative transcriptomic analysis across tissues, cell types, and species**
>
> Erik Bot & José Davila-Velderrain
>
> _eLife_ April 15th, 2026. DOI: [https://doi.org/10.7554/eLife.110430.1](https://elifesciences.org/reviewed-preprints/110430).

[Bot and Davila-Velderrain, 2025]: https://elifesciences.org/reviewed-preprints/110430
[article]: https://elifesciences.org/reviewed-preprints/110430
[vignette]: https://github.com/davilavelderrainlab/GeneSLand/blob/main/vignettes/GeneSLand.Rmd
[issues]: https://github.com/davilavelderrainlab/GeneSLand/issues
