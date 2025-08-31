# INSPECT-SR Framework

## Project Description

The INSPECT-SR (INtegrity and Systematic Review Assessment) Framework is a cutting-edge research methodology that evaluates the effectiveness of Large Language Models (LLMs) in assessing the trustworthiness of Randomized Controlled Trials (RCTs) for systematic reviews.

## Key Features

- **Comprehensive Assessment**: Evaluates four critical trustworthiness dimensions
- **Multi-LLM Comparison**: Tests Claude Sonnet 4, ChatGPT 5, and Gemini 2.5 Pro
- **Statistical Rigor**: Uses Cohen's κ, linear-weighted κ, and Cramér's V
- **Publication Ready**: Generates professional visualizations and tables
- **Reproducible**: Complete R workflow with session information

## Research Impact

This framework addresses a critical gap in systematic review methodology by:
1. **Automating Quality Assessment**: Reducing manual workload for researchers
2. **Standardizing Evaluation**: Providing consistent assessment criteria
3. **Improving Efficiency**: Accelerating systematic review processes
4. **Enhancing Reliability**: Validating AI-assisted research methods

## Target Audience

- **Systematic Review Researchers**: Looking to improve efficiency
- **Research Methodologists**: Developing AI-assisted tools
- **Evidence Synthesis Teams**: Seeking automation solutions
- **Academic Institutions**: Teaching systematic review methods
- **Research Integrity Researchers**: Studying AI reliability

## Technical Specifications

- **Language**: R (with comprehensive package ecosystem)
- **Input**: Excel data with manual and LLM assessments
- **Output**: Publication-ready figures, tables, and data files
- **Compatibility**: Cross-platform (Windows, macOS, Linux)
- **Dependencies**: 20+ R packages for robust analysis

## Repository Structure

```
├── final.R                    # Main analysis pipeline
├── json_to_csv.R             # Data conversion utility
├── enhanced_analysis.R       # Enhanced analysis functions
├── .gitignore                # Git ignore rules
├── README.md                 # Comprehensive documentation
├── LICENSE                   # MIT License
├── DESCRIPTION.md            # This file
└── results/                  # Generated outputs
```

## Getting Started

1. **Clone the repository**
2. **Install R dependencies**
3. **Prepare your data** (Excel format)
4. **Run the analysis** with `source("final.R")`
5. **Review outputs** in the results/ directory

## Citation

```
Dhaliwal, G. (2024). INSPECT-SR Framework: Large Language Model Assessment 
of RCT Trustworthiness in Systematic Reviews. GitHub Repository.
```

## License

MIT License - Open source for research and educational use.

## Contributing

We welcome contributions! Please see the contributing guidelines in the README.

---

*This framework represents a significant advancement in systematic review methodology, combining traditional research rigor with cutting-edge AI capabilities.*
