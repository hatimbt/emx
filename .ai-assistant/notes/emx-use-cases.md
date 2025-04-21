# EMX Use Cases and Support Priorities

## Currently Supported Use Cases

### 1. Standard Deployment (Priority: Essential)
- **Description**: Using `emx` launcher with configuration from Nix store
- **Execution**: `emx` (standard launcher)
- **Config Source**: Immutable Nix store
- **Emacs Binary**: Nix-managed with included packages
- **Directory Structure**: XDG-compliant
- **Reproducibility**: High (pinned through Nix)
- **Use Case**: Day-to-day Emacs usage in a stable environment

### 2. Development with Local Source (Priority: Essential)
- **Description**: Using `emx` launcher with configuration from local directory
- **Execution**: `emx` (standard launcher)
- **Config Source**: Local directory (~/src/emx)
- **Emacs Binary**: Nix-managed with included packages
- **Directory Structure**: XDG-compliant
- **Reproducibility**: Medium (packages pinned, config in flux)
- **Use Case**: Active development of the EMX configuration
- **Note**: Enabled through the `localPath` option in Home Manager

### 3. Direct Init Directory (Priority: High)
- **Description**: Running Emacs directly from the EMX source directory
- **Execution**: `cd ~/src/emx && emacs --init-directory .`
- **Config Source**: Current directory
- **Emacs Binary**: System Emacs or Nix Emacs
- **Directory Structure**: Mixed (source = config dir)
- **Reproducibility**: Medium (system-dependent)
- **Use Case**: Rapid testing and development without rebuilding
- **Note**: Requires fallback detection in early-init.el and init.el

### 4. Clean Environment Testing (Priority: Medium)
- **Description**: Testing in a clean Emacs without initialization
- **Execution**: `emacs -Q` then manually load files
- **Config Source**: Manual loading (`load-file "~/src/emx/init.el"`)
- **Emacs Binary**: Any available Emacs
- **Directory Structure**: Ad-hoc
- **Reproducibility**: Low (depends on manual steps)
- **Use Case**: Debugging, isolated testing, feature development
- **Note**: Requires robust fallback detection for all variables

### 5. Direct File Evaluation (Priority: Medium)
- **Description**: Evaluating configuration files directly in an Emacs buffer
- **Execution**: Open file, use `eval-buffer` or `eval-region`
- **Config Source**: Local files being edited
- **Emacs Binary**: Any Emacs instance
- **Directory Structure**: Determined by fallbacks
- **Reproducibility**: Low (environment-dependent)
- **Use Case**: Development, testing, and troubleshooting

### 6. Integration with Other Distributions (Priority: Low)
- **Description**: Using EMX components within another Emacs configuration
- **Execution**: Manual loading of specific modules
- **Config Source**: EMX modules selectively loaded
- **Emacs Binary**: Any Emacs instance
- **Directory Structure**: Foreign
- **Reproducibility**: Very low
- **Use Case**: Cherry-picking EMX features for personal configurations

## Support Philosophy

The EMX support philosophy aligns with these guiding principles (original statement preserved for historical context):

> "We should always have a reproducible (albeit less up-to-date) version. We should have the least friction during development. The Emacs environment is fundamentally a dynamic lisp computing environment. Our goal here is to tie down some of the rough edges with the emacs painpoints and UX issues. We don't aim to make Emacs rigid. We want a controlled dynamic environment. We want an environment that is not brittle. We want to be able to be fearless when using the dynamic lisp environment provided by Emacs."

### EMX Philosophy Expanded

EMX balances two fundamental aspects of Emacs configuration:

1. **Reproducibility**: Using Nix to provide a consistent, versioned foundation for the Emacs binary, core packages, and directory structure.

2. **Dynamic development**: Preserving Emacs' interactive Lisp environment that enables runtime customization, experimentation, and rapid iteration.

This creates a bidirectional approach to stability:

- **Bottom-up reliability**: The foundation (Emacs binary, system libraries, directory structure) is stable and reproducible through Nix.
  
- **Top-down flexibility**: The user-facing layers (Elisp modules, customizations, package configurations) remain dynamic and adaptable.

The resulting tiered stability model:

- **System level**: Highly reproducible (Emacs binary, C dependencies)
- **Core configuration**: Moderately reproducible (directory structure, essential packages)
- **User configuration**: Highly dynamic (module customization, personal preferences)

EMX provides a stable foundation while preserving Emacs' dynamic nature. When configuration changes cause problems, users can revert to the known-good Nix-managed baseline while retaining their experimental work.

The implementation focuses on:
- Clear separation between reproducible and dynamic components
- Minimal friction during development
- Consistent behavior across different contexts
- Graceful fallbacks for non-standard use cases

### Priority Framework

Based on this philosophy, our priority levels mean:

1. **Essential**: Must work flawlessly; core functionality
2. **High**: Should work reliably; important for common workflows
3. **Medium**: Should work in most cases; useful for development
4. **Low**: Nice to have; edge cases with limited support

### Implementation Strategy

The current implementation strategy provides this support through:

1. **Layered Shims**: Configuration loaded through progressive layers
2. **Variable Fallbacks**: Graceful degradation for missing variables
3. **Directory Detection**: Smart detection of source directories
4. **XDG Compliance**: Standard directory organization
5. **Development Mode**: Support for localPath development
6. **Backward Compatibility**: Aliases for legacy variable names

When implementing new features, consider how they affect each use case, prioritizing the higher-priority cases while not unnecessarily breaking lower-priority ones.