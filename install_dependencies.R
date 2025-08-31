#!/usr/bin/env Rscript

# =============================================================================
# INSPECT-SR FRAMEWORK - DEPENDENCY INSTALLER
# =============================================================================
# 
# This script installs all required R packages for the INSPECT-SR framework.
# Run this script before using the main analysis functions.
#
# Author: Gagan Dhaliwal
# Date: 2024
# =============================================================================

cat("🔧 Installing INSPECT-SR Framework Dependencies\n")
cat("==============================================\n\n")

# Function to install packages safely
safe_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("📦 Installing", pkg, "...\n")
      tryCatch({
        install.packages(pkg, dependencies = TRUE)
        cat("✅", pkg, "installed successfully\n")
      }, error = function(e) {
        cat("❌ Failed to install", pkg, ":", e$message, "\n")
      })
    } else {
      cat("✅", pkg, "already installed\n")
    }
  }
}

# Core data manipulation packages
cat("📊 Installing core data manipulation packages...\n")
core_packages <- c(
  "readxl",      # Excel file reading
  "dplyr",       # Data manipulation
  "tidyr",       # Data tidying
  "purrr",       # Functional programming
  "stringr",     # String manipulation
  "tibble",      # Modern data frames
  "readr"        # Fast file reading
)
safe_install(core_packages)

# Visualization packages
cat("\n🎨 Installing visualization packages...\n")
viz_packages <- c(
  "ggplot2",     # Grammar of graphics
  "scales",      # Scale functions
  "patchwork",   # Plot composition
  "viridis",     # Color palettes
  "ggrepel"      # Label positioning
)
safe_install(viz_packages)

# Table and output packages
cat("\n📋 Installing table and output packages...\n")
table_packages <- c(
  "gt",          # Grammar of tables
  "kableExtra",  # Table styling
  "openxlsx"     # Excel output
)
safe_install(table_packages)

# Statistical analysis packages
cat("\n📈 Installing statistical analysis packages...\n")
stats_packages <- c(
  "boot"         # Bootstrap methods
)
safe_install(stats_packages)

# Font and graphics packages
cat("\n🔤 Installing font and graphics packages...\n")
font_packages <- c(
  "sysfonts",    # System fonts
  "showtext"     # Font rendering
)
safe_install(font_packages)

# High-quality output packages
cat("\n🖼️ Installing high-quality output packages...\n")
output_packages <- c(
  "ragg",        # High-quality graphics
  "svglite"      # SVG output
)
safe_install(output_packages)

# Optional packages (for enhanced functionality)
cat("\n🔍 Installing optional packages...\n")
optional_packages <- c(
  "webshot2",    # HTML to image conversion
  "rsvg"         # SVG to PNG conversion
)

# Try to install optional packages but don't fail if they don't install
for (pkg in optional_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("📦 Installing optional package", pkg, "...\n")
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      cat("✅", pkg, "installed successfully\n")
    }, error = function(e) {
      cat("⚠️  Optional package", pkg, "failed to install:", e$message, "\n")
      cat("   This won't affect core functionality\n")
    })
  } else {
    cat("✅", pkg, "already installed\n")
  }
}

# Verify installations
cat("\n🔍 Verifying package installations...\n")
required_packages <- c(core_packages, viz_packages, table_packages, stats_packages, 
                      font_packages, output_packages)

missing_packages <- c()
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) == 0) {
  cat("🎉 All required packages installed successfully!\n")
  cat("✅ You can now run the INSPECT-SR framework\n")
} else {
  cat("❌ Some packages failed to install:\n")
  for (pkg in missing_packages) {
    cat("   -", pkg, "\n")
  }
  cat("\n💡 Try installing these packages manually:\n")
  cat("   install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n")
}

# Load key packages to verify they work
cat("\n🧪 Testing package loading...\n")
tryCatch({
  library(dplyr)
  library(ggplot2)
  library(gt)
  cat("✅ Core packages loaded successfully\n")
}, error = function(e) {
  cat("❌ Error loading core packages:", e$message, "\n")
})

cat("\n🚀 INSPECT-SR Framework dependencies installation complete!\n")
cat("📚 Next steps:\n")
cat("   1. Review the README.md for usage instructions\n")
cat("   2. Prepare your data in Excel format\n")
cat("   3. Run the analysis with: source('final.R')\n")
cat("\n💡 For help, check the documentation or create an issue on GitHub\n")
