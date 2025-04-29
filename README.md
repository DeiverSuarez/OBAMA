
# OBAMA

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

##🧬 What is OBAMA?

**OBAMA** (*Optimization-Based Analysis of Micro Arrays*) is an R package that includes an interactive **Shiny application** designed to assist researchers in the analysis of **gene expression data**.

This tool integrates **three complementary optimization-based methods** to support different stages of transcriptomic analysis:

1. **MCO (Multi-Criteria Optimization):** Selects genes that show the most significant changes in expression across conditions, optimizing multiple criteria.
2. **MST (Minimum Spanning Tree):** Constructs a network that highlights relationships between gene products based on maximum correlation, enabling the identification of interaction structures among deregulated genes.
3. **OGF (Ontology-Guided Filtering):** Groups genes and biological terms (such as Gene Ontology: Biological Processes) into highly probable functional groups, helping interpret the biological relevance of deregulated genes.

Together, these methods allow for robust gene selection, structural analysis of gene-gene associations, and biological interpretation based on functional annotation.

OBAMA is particularly suitable for analyzing microarray or RNA-seq datasets from individual studies or performing meta-analyses across multiple datasets.

---

## 📦 Installation

Before installing OBAMA, please ensure that the archived version of the optrees package is installed manually, as it is required to properly run the MST (Minimum Spanning Tree) analysis:

```r
# Step 1: Download and install 'optrees'
install.packages("https://cran.r-project.org/src/contrib/Archive/optrees/optrees_1.0.tar.gz",
                 repos = NULL, type = "source")

# Step 2: Install OBAMA from GitHub
devtools::install_github("DeiverSuarez/OBAMA")
```

---

## 🚀 Getting Started

To launch the OBAMA Shiny application:

```r
library(OBAMA)
run_app()
```

Once the app is running, upload your input files in `.csv` format containing gene expression matrices. You will be able to compare conditions, visualize deregulated genes, and construct optimized correlation networks.

---

## 📁 Input Data Format

Input files must be in `.csv` or `.tsv` format, with the following structure:

- Each **row represents a sample**.
- Each **column represents a gene**, with gene expression values.
- The **first two columns** must be:

  1. `geo_accession`: unique sample identifier (e.g., GSM650656).  
  2. `disease.stat`: associated biological condition, such as `control` or `disease`.

The `disease.stat` column is **required** to perform differential expression comparisons between groups.

### Example of file structure:

| geo_accession | disease.stat | A1BG     | A1CF     | A2M      | ... |
|---------------|--------------|----------|----------|----------|-----|
| GSM650656     | disease      | 192.72   | 94.82    | 123.33   | ... |
| GSM650657     | control      | 241.33   | 120.10   | 142.54   | ... |
| GSM650658     | disease      | 213.99   | 130.45   | 159.91   | ... |

Example dataset: [See `/data/` folder]

---

## 📊 Example Use Case

```r
# Example: Load test dataset and launch app
library(OBAMA)
run_app(data_path = system.file("extdata", "test_data.csv", package = "OBAMA"))
```

Expected output includes:
- Ranked gene list by optimization score
- Correlation-based gene network
- Enrichment links (e.g., STRING)

---

## 📚 License

This package is licensed under the MIT License.  
See the [LICENSE.md](LICENSE.md) file for more details.

---

## 📌 Citation

If you use OBAMA in your research, please cite:

> Suárez-Gómez, D. et al. *OBAMA: Optimization-Based Analysis of Micro Arrays*. (Manuscript in preparation).  
> [Add final reference once published]

---

## 🧪 Version and Contact

**Version:** 1.0  
**Contact:** [deiver.suarez@upr.edu](mailto:deiver.suarez@upr.edu)

For bug reports or feature requests, please use the [GitHub Issues page](https://github.com/DeiverSuarez/OBAMA/issues).

---

## 📂 FAIR Compliance

This repository follows the FAIR principles by including:
- Clear metadata and documentation
- Example datasets and usage
- Interoperability with standard data formats (CSV)
- Reusability via open-source licensing and citation information
