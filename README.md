
# OBAMA

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## 🧬 What is OBAMA?

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
  2. `disease.state`: associated biological condition, such as `control` or `disease`.

The `disease.state` column is **required** to perform differential expression comparisons between groups.

### Example of file structure:

| geo_accession | disease.stat | A1BG     | A1CF     | A2M      | ... |
|---------------|--------------|----------|----------|----------|-----|
| GSM650656     | disease      | 192.72   | 94.82    | 123.33   | ... |
| GSM650657     | control      | 241.33   | 120.10   | 142.54   | ... |
| GSM650658     | disease      | 213.99   | 130.45   | 159.91   | ... |

 **Examples datasets: [See `OBAMA/data-raw/` folder]**

---

## 📊 Example Use Case: How to Run MCO (Multi-Criteria Optimization)

The MCO module helps identify genes with significant expression changes across conditions by optimizing multiple performance criteria. You can access this feature directly from the OBAMA Shiny interface.


## 📖 Steps to Run MCO:
  1. **Launch the Application:**
  
```r
library(OBAMA)
run_app()
```

### 2️⃣ Navigate to the MCO Section

Use the top navigation menu to access the **MCO** functionality.

---

### 3️⃣ Configure Analysis Parameters

- **Number of Performance Metrics:**
  - Select **Two** or **Three** metrics depending on your analysis.
  - Choose the desired performance metrics for each (e.g., *Median*, *Mean*).

- **Number of Datasets:**
  - Indicate whether you are analyzing **one** or **multiple datasets**.

- **Number of Frontiers:**
  - Define the number of **Pareto frontiers** to explore (default: **10**).

---

### 📂 Upload Your Data

- Click **Browse** under **Gene Expression Dataset Example: GSE35974_SCZ.csv**.
- Upload a `.csv` file following the required input format.

---

### ▶️ Run the Analysis

- Click the **Run** button to start the MCO analysis.

---

### 📊 Explore Results

Navigate through the following tabs:

- **SummaryData**: Review summary statistics.
- **Frontiers**: Examine gene sets identified at different optimization frontiers.
- **Frontiers-plot**: Visualize the Pareto frontiers interactively.
- **Visualization**: Explore graphical representations of selected genes.

---

### 💾 Export Results

- Click **Save My Results** To download the gene set with the highest expression changes for further analysis.

---

## 🌐 Example Use Case: MST (Minimum Spanning Tree)

The MST module identifies key correlation structures among a set of deregulated genes, constructing a **minimum spanning tree** that reveals highly connected nodes (genes) within the expression network.

This is especially useful to detect possible regulatory hubs or functionally linked genes based on similarity in expression profiles.

### 📖 Steps to Run MST:

1. **Launch the OBAMA App:**
```r
library(OBAMA)
run_app()
```

1. **Navigate to the MST Section:**  
   - Use the top menu bar and click on **MST**.

2. **Upload Required Data:**  
   - **Gene Expression Data:** Upload a `.csv` file containing gene expression values (Example: GSE35974_SCZ.csv).  
   - **Genes of Interest:** Upload a list of genes to be included in the network. This file must contain a single column with gene names (e.g., Gene_of_interest_GSE35974_SCZ.csv).

3. **Run the Analysis:**  
   - Click the **Run** button to compute the correlation matrix and generate the MST.

4. **Explore Results:**  
   Navigate through the available tabs:
   - **SummaryData:** Overview of input data and correlation statistics.
   - **MST table:** Displays the edges (connections) in the MST with corresponding correlation values.
   - **MST diagram:** An interactive network visualization showing the structure of gene-gene relationships.

5. **Export Results:**  
   - Click **Save My MST Results** to download the MST output for further exploration or visualization in external tools.


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
