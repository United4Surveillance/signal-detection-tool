## CONTRIBUTING Guidelines

Welcome to the United4Surveillance signal detection tool repository! We're excited to have you contribute to our open-source project. Before you get started, please take a moment to read through the following guidelines to ensure a smooth and productive collaboration.

### Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [How to Contribute](#how-to-contribute)
    - [Reporting Bugs](#reporting-bugs)
    - [Suggesting Enhancements](#suggesting-enhancements)
    - [Working on Issues](#working-on-issues)
    - [Submitting Pull Requests](#submitting-pull-requests)
    - [Reviewing Pull Requests](#reviewing-pull-requests)
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
- Use our [bug report template](ISSUE_TEMPLATE/bug-report.md) to provide necessary details about the issue, including steps to reproduce and expected behavior.

#### Suggesting Enhancements

- If you have an idea for an enhancement, open an issue using our [feature request template](ISSUE_TEMPLATE/feature-request.md).
- Describe the enhancement in detail, including its use case and benefits.

#### Working on Issues

- You may assign yourself to an issue if nobody else is assigned yet and you are ready to start working on the issue
- Make sure to solve the issue according to the specified requirements
- If you don't agree with the definition of the issue or problems arise while working on it, comment on the issue to clarify.
  
#### Submitting Pull Requests

- If you are not a maintainer or owner of the repository, please fork the repository and create a new branch from `main` or the relevant feature branch.
- Follow our [pull request template](PULL_REQUEST_TEMPLATE.md) when submitting a pull request.
- Ensure your code adheres to our [coding guidelines](#coding-guidelines).
- Make sure that your branch entails all relevant previously merged changes and your code is compatible to what is on the main branch. Having started from an old state is not an excuse to push outdated code.
- Include tests if applicable, and ensure all tests pass.

#### Reviewing Pull Requests

- A pull request may only be merged after a review by another contributor
- You may assign yourself as an reviewer of a pull request, if you are not its author
- Please take a close look at the commits and the issue related to the PR
- Accept the PR if it solves the problem outlined in the feature request issue, adheres to the coding guidelines and is up to date on the current main HEAD.
- If any tests fail, reject the PR
- Use github's PR review functionality and provide constructive feedback

### Coding Guidelines

- Follow the [tidyverse style guide](https://style.tidyverse.org/) to maintain code consistency. The RStudio addin [styler](https://styler.r-lib.org/) can help you with that.
- As an enhancement to the tidyverse style guide, we will make explicit use of namespaces in order to avoid conflicts and improve clarity (i.e. write dplyr::filter() and stats::filter())
- We stick to this pipe operator %>%
- Write clear and descriptive code comments
- For documentation use [roxygen comments](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html).
- Keep dependencies updated and minimize external dependencies where possible.
- For repository structure use [devtools package skeleton](https://bookdown.org/rdpeng/RProgDA/the-devtools-package.html)
- The code should be compatible/runnable with R-version 4.0.2

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
- Use the [testthat](https://testthat.r-lib.org/) package for writing tests


Thank you for contributing to the United4Surveillance signal detection tool!
