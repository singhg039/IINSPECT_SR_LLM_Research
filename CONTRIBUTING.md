# Contributing to INSPECT-SR Framework

Thank you for your interest in contributing to the INSPECT-SR Framework! This document provides guidelines for contributing to this research project.

## 🤝 How to Contribute

### Types of Contributions

We welcome various types of contributions:

- **Bug Reports**: Report issues or bugs you encounter
- **Feature Requests**: Suggest new features or improvements
- **Code Contributions**: Submit code improvements or new functionality
- **Documentation**: Improve or expand documentation
- **Testing**: Help test the framework with different datasets
- **Research Applications**: Apply the framework to new research areas

## 🚀 Getting Started

### Prerequisites

- R (version 4.0 or higher)
- Basic knowledge of R programming
- Understanding of systematic review methodology
- Familiarity with statistical analysis concepts

### Development Setup

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/Analysis.git
   cd Analysis
   ```
3. **Install dependencies**:
   ```r
   source("install_dependencies.R")  # If available
   # Or manually install required packages
   ```

## 📝 Contribution Guidelines

### Code Style

- **R Style**: Follow [tidyverse style guide](https://style.tidyverse.org/)
- **Naming**: Use descriptive variable and function names
- **Comments**: Add clear comments explaining complex logic
- **Documentation**: Document all functions and their parameters

### Commit Messages

Use clear, descriptive commit messages:

```
feat: add new visualization for agreement analysis
fix: correct calculation error in Cohen's kappa
docs: update README with installation instructions
test: add unit tests for statistical functions
```

### Pull Request Process

1. **Create a feature branch** from main:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following the guidelines above

3. **Test your changes**:
   ```r
   source("final.R")  # Ensure no errors
   # Test with sample data
   ```

4. **Commit your changes**:
   ```bash
   git add .
   git commit -m "feat: description of your changes"
   ```

5. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

6. **Create a Pull Request** with:
   - Clear description of changes
   - Reference to any related issues
   - Screenshots if UI changes
   - Test results if applicable

## 🧪 Testing

### Before Submitting

- [ ] Code runs without errors
- [ ] All tests pass (if applicable)
- [ ] Documentation is updated
- [ ] Code follows style guidelines
- [ ] Changes are tested with sample data

### Testing Checklist

- **Functionality**: Does the code do what it's supposed to?
- **Error Handling**: Are errors handled gracefully?
- **Performance**: Does it perform reasonably with large datasets?
- **Compatibility**: Does it work across different R versions?

## 📚 Documentation

### Code Documentation

- Document all functions with Roxygen2 comments
- Include examples in documentation
- Explain complex algorithms or statistical methods
- Add inline comments for non-obvious code

### User Documentation

- Update README.md for new features
- Add usage examples
- Document any new configuration options
- Update installation instructions if needed

## 🔍 Review Process

### Pull Request Review

1. **Automated Checks**: Ensure CI/CD passes
2. **Code Review**: At least one maintainer must approve
3. **Testing**: Changes are tested with sample data
4. **Documentation**: All changes are properly documented

### Review Criteria

- **Correctness**: Does the code work as intended?
- **Efficiency**: Is the code reasonably efficient?
- **Maintainability**: Is the code easy to understand and maintain?
- **Documentation**: Is the code properly documented?

## 🐛 Bug Reports

### Reporting Bugs

When reporting bugs, please include:

- **Description**: Clear description of the problem
- **Reproduction Steps**: Steps to reproduce the issue
- **Expected vs Actual**: What you expected vs what happened
- **Environment**: R version, OS, package versions
- **Sample Data**: Minimal dataset to reproduce the issue

### Bug Report Template

```markdown
**Bug Description**
Brief description of the issue

**Steps to Reproduce**
1. Step 1
2. Step 2
3. Step 3

**Expected Behavior**
What should happen

**Actual Behavior**
What actually happened

**Environment**
- R version: X.X.X
- OS: Windows/macOS/Linux
- Package versions: list relevant packages

**Additional Information**
Any other relevant details
```

## 💡 Feature Requests

### Suggesting Features

When suggesting features:

- **Clear Description**: Explain what you want to achieve
- **Use Case**: Describe how it would be useful
- **Implementation Ideas**: Suggest how it might be implemented
- **Priority**: Indicate how important this is to you

## 📞 Getting Help

### Questions and Discussion

- **GitHub Issues**: Use issues for questions and discussions
- **Pull Requests**: Comment on PRs for specific feedback
- **Documentation**: Check existing documentation first

## 🏆 Recognition

### Contributors

Contributors will be recognized in:
- README.md contributors section
- Release notes
- Project documentation

### Types of Recognition

- **Code Contributors**: Direct code contributions
- **Documentation Contributors**: Documentation improvements
- **Testing Contributors**: Testing and bug reporting
- **Research Contributors**: Research applications and validation

## 📋 Code of Conduct

### Our Standards

- Be respectful and inclusive
- Focus on constructive feedback
- Help others learn and grow
- Respect different perspectives and experiences

### Enforcement

- Unacceptable behavior will not be tolerated
- Maintainers will address issues promptly
- Violations may result in temporary or permanent exclusion

## 🎯 Current Priorities

### High Priority

- Bug fixes and stability improvements
- Performance optimizations
- Documentation improvements
- Testing coverage expansion

### Medium Priority

- New visualization types
- Additional statistical methods
- User interface improvements
- Performance monitoring tools

### Low Priority

- Experimental features
- Nice-to-have improvements
- Cosmetic changes

## 🙏 Thank You

Thank you for contributing to the INSPECT-SR Framework! Your contributions help advance systematic review methodology and make research more accessible to the community.

---

*Together, we can build better tools for evidence synthesis and research integrity assessment.*
