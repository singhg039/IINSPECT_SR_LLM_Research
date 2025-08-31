# INSPECT-SR Framework: Large Language Model Assessment of RCT Trustworthiness

## 🎯 Overview

The INSPECT-SR (INtegrity and Systematic Review Assessment) Framework is a comprehensive methodology for evaluating the trustworthiness of Randomized Controlled Trials (RCTs) using Large Language Models (LLMs) in systematic reviews.

## 🔬 Research Purpose

This framework addresses the critical need for efficient, reliable assessment of study trustworthiness in systematic reviews by:
- Comparing manual expert assessment (gold standard) with LLM performance
- Evaluating agreement levels using Cohen's κ and linear-weighted κ
- Assessing effect sizes using Cramér's V
- Providing systematic review methodology for AI-assisted quality assessment

## 📊 Key Components

### Primary Assessment Checks
- **Retraction Detection**: Identification of retracted studies
- **Expression of Concern**: Detection of published concerns
- **Team Integrity**: Assessment of author team credibility
- **Registration Timing**: Evaluation of trial registration compliance

### LLM Models Evaluated
- Claude Sonnet 4
- ChatGPT 5
- Gemini 2.5 Pro

## 🚀 Quick Start

### Prerequisites
```r
# Required R packages
install.packages(c(
  "readxl", "dplyr", "tidyr", "purrr", "stringr", "tibble",
  "ggplot2", "boot", "scales", "patchwork", "gt", "kableExtra",
  "openxlsx", "viridis", "sysfonts", "showtext", "ragg",
  "webshot2", "rsvg", "readr", "svglite", "ggrepel"
))
```

### Basic Usage
```r
# Run the complete analysis
source("final.R")

# Convert JSON responses to CSV (if needed)
source("json_to_csv.R")
```

## 📁 Repository Structure

```
Analysis/
├── final.R                          # Main analysis script
├── json_to_csv.R                    # JSON to CSV converter
├── enhanced_analysis.R              # Enhanced analysis functions
├── INSPECT_SR Checks Gagan.xlsx     # Sample data
├── .gitignore                       # Git ignore rules
├── README.md                        # This file
└── results/                         # Generated outputs
    ├── comprehensive_publication_workbook.xlsx
    ├── BEST_publication_table.tex
    ├── enhanced_summary.csv
    └── ...
```

## 📈 Outputs Generated

### Professional Visualizations
- **Figure 1**: Binary Check Agreement Analysis (Cohen's κ)
- **Figure 2**: Ordinal Check Agreement Analysis (Linear-Weighted κ)
- **Figure 3**: Effect Size Analysis (Cramér's V)
- **Figure 5**: Sample Characteristics Overview
- **Figure 6A**: Manual Gold Standard Assessment
- **Figure 6B**: LLM Performance Evaluation

### Publication-Ready Tables
- LaTeX tables for journal submission
- HTML tables for quick viewing
- Comprehensive Excel workbook for reviewers

### Data Files
- Enhanced summary statistics
- Binary diagnostic metrics
- Missing data patterns
- Session information for reproducibility

## 🔧 Configuration

### Input Data Format
The framework expects Excel data with columns for:
- Manual assessment results (gold standard)
- LLM assessment results
- Study identifiers and metadata

### Output Customization
- Adjust plot dimensions for different journal requirements
- Modify color schemes and themes
- Customize agreement level thresholds

## 📚 Methodology

### Statistical Analysis
- **Cohen's κ**: For binary agreement assessment
- **Linear-weighted κ**: For ordinal agreement assessment
- **Cramér's V**: For effect size analysis
- **Bootstrap confidence intervals**: For robust statistical inference

### Quality Assessment
- Agreement level thresholds: Excellent (≥0.80), Good (≥0.60), Fair (≥0.40), Poor (≥0.20)
- Effect size interpretation: Large (≥0.5), Medium (≥0.3), Small (≥0.1)

## 🎨 Design Features

- Professional publication-ready visualizations
- Consistent color schemes and typography
- High-resolution exports (300 DPI)
- Word-optimized dimensions and proportions
- Comprehensive captions and annotations

## 📖 Citation

If you use this framework in your research, please cite:

```
Dhaliwal, G. (2024). INSPECT-SR Framework: Large Language Model Assessment 
of RCT Trustworthiness in Systematic Reviews. [GitHub Repository]
```

## 🤝 Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🔗 Related Work

- Systematic Review Methodology
- AI-Assisted Research Assessment
- Research Integrity Evaluation
- Evidence Synthesis Automation

## 📞 Contact

- **Author**: Gagan Dhaliwal
- **Email**: [Your Email]
- **GitHub**: [Your GitHub Username]

## 🙏 Acknowledgments

- Systematic review methodology community
- Open-source R package developers
- Research integrity researchers

---

**Note**: This framework is designed for research and educational purposes. Always validate results and use appropriate statistical methods for your specific research questions.
