## CONTRIBUTING Guidelines

Welcome to the United4Surveillance signal detection tool repository! We're excited to have you contribute to our open-source project. Before you get started, please take a moment to read through the following guidelines to ensure a smooth and productive collaboration.

### Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [How to Contribute](#how-to-contribute)
    - [Reporting Bugs](#reporting-bugs)
    - [Suggesting Enhancements](#suggesting-enhancements)
    - [Submitting Pull Requests](#submitting-pull-requests)
3. [Coding Guidelines](#coding-guidelines)
4. [Commit Message Guidelines](#commit-message-guidelines)
5. [Branching Strategy](#branching-strategy)
6. [Documentation](#documentation)
7. [Testing](#testing)

### Code of Conduct

Please note that this project adheres to the [Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project, you agree to abide by its terms.

### How to Contribute

We welcome contributions from the community! Whether you're reporting a bug, suggesting an enhancement, or submitting a pull request, your input is valuable to us. Please follow the guidelines below when contributing:

#### Reporting Bugs

- Before submitting a new bug report, please check the existing issues to see if the bug has already been reported.
- Use our [bug report template](.github/ISSUE_TEMPLATE/bug-report.md) to provide necessary details about the issue, including steps to reproduce and expected behavior.

#### Suggesting Enhancements

- If you have an idea for an enhancement, open an issue using our [feature request template](.github/ISSUE_TEMPLATE/feature-request.md).
- Describe the enhancement in detail, including its use case and benefits.

#### Submitting Pull Requests

- If you are not a maintainer or owner of the repository, please fork the repository and create a new branch from `main` or the relevant feature branch.
- Follow our [pull request template](.github/PULL_REQUEST_TEMPLATE.md) when submitting a pull request.
- Ensure your code adheres to our [coding guidelines](#coding-guidelines).
- Include tests if applicable, and ensure all tests pass.

### Coding Guidelines

- Follow the [tidyverse style guide](https://style.tidyverse.org/) to maintain code consistency. The RStudio addin [styler](https://styler.r-lib.org/) can help you with that.
- As an enhancement to the tidyverse style guide, we will make explicitl use of namespaces in order to avoid conflicts and improve clarity (i.e. write dplyr::filter() and stats::filter()) 
- Write clear and descriptive code comments.
- Keep dependencies updated and minimize external dependencies where possible.

### Commit Message Guidelines

- Use meaningful commit messages to convey the purpose of your changes.
- If longer descriptions are necessary, separate the subject from the body with a blank line for clarity.
- Reference related issues or pull requests where necessary.

### Branching Strategy

- Create feature branches for new features or enhancements and bug branches for fixing issues.
- Branch names should be descriptive and follow the format: `feature/your-feature-name`, `bug/issue-number`.
- Ideally, finished work from new branches should be reviewed before merging (see pull requests), even if changes were made by a maintainer or owner. If there are only minor changes, you may merge without a review. Use common sense.

### Documentation

- Maintain and update the project's [documentation](docs/) as needed.
- Document new features, changes, and usage instructions.

### Testing

- Write unit tests and integration tests for new features and bug fixes.
- Ensure all existing tests pass before submitting a pull request.
- Include testing instructions in the pull request if necessary.


Thank you for contributing to the United4Surveillance signal detection tool!
