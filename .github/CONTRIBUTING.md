## CONTRIBUTING Guidelines

Welcome to the United4Surveillance signal detection tool repository! We're excited to have you contribute to our open-source project. Before you get started, please take a moment to read through the following guidelines to ensure a smooth and productive collaboration.

### Table of Contents

1.  [Code of Conduct](#code-of-conduct)
2.  [How to Contribute](#how-to-contribute)
    -   [Reporting Bugs](#reporting-bugs)
    -   [Suggesting Enhancements](#suggesting-enhancements)
    -   [Working on Issues](#working-on-issues)
    -   [Submitting Pull Requests](#submitting-pull-requests)
    -   [Reviewing Pull Requests](#reviewing-pull-requests)
3.  [Coding Guidelines](#coding-guidelines)
4.  [Commit Message Guidelines](#commit-message-guidelines)
5.  [Branching Strategy](#branching-strategy)
6.  [Documentation](#documentation)
7.  [Testing](#testing)

### Code of Conduct {#code-of-conduct}

Please note that this project adheres to the [Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project, you agree to abide by its terms.

### How to Contribute {#how-to-contribute}

We welcome contributions from the community! Whether you're reporting a bug, suggesting an enhancement, or submitting a pull request, your input is valuable to us. Please follow the guidelines below when contributing:

#### Reporting Bugs {#reporting-bugs}

-   Before submitting a new bug report, please check the existing issues to see if the bug has already been reported.
-   Use our [bug report template](ISSUE_TEMPLATE/bug-report.md) to provide necessary details about the issue, including steps to reproduce and expected behavior.

#### Suggesting Enhancements {#suggesting-enhancements}

-   If you have an idea for an enhancement, open an issue using our [feature request template](ISSUE_TEMPLATE/feature-request.md).
-   Describe the enhancement in detail, including its use case and benefits.

#### Working on Issues {#working-on-issues}

-   You may assign yourself to an issue if nobody else is assigned yet and you are ready to start working on the issue
-   Make sure to solve the issue according to the specified requirements
-   If you don't agree with the definition of the issue or problems arise while working on it, comment on the issue to clarify.

#### Submitting Pull Requests {#submitting-pull-requests}

-   If you are not a maintainer or owner of the repository, please fork the repository and create a new branch from `main` or the relevant feature branch.
-   Switch the branch to which to merge the Pull Request from the default `main` to `develop`. `main` contains the current released code and we are working on `develop`.
-   Ensure your code adheres to our [coding guidelines](#coding-guidelines).
-   Make sure that your branch entails all relevant previously merged changes and your code is compatible to what is on the main branch. Having started from an old state is not an excuse to push outdated code.
-   Include tests if applicable, and ensure all tests pass.
-   Ensure that all checks which are automatically performed on your PR by Github Actions succeeded. If they did not understand the issue and fix the problem and commit again.
-   Please write a short bullet point what you fixed, added etc. into the `NEWS.md` file above the header of the latest version. This helps us to keep an overview of all changes for the next release. If it is a really minor change not worth mentioning you do not need to write a note.

#### Reviewing Pull Requests {#reviewing-pull-requests}

-   A pull request may only be merged after a review by another contributor
-   You may assign yourself as an reviewer of a pull request, if you are not its author
-   Please take a close look at the commits and the issue related to the PR
-   Start the app and run it with the test dataset. Check whether all main functionalities, such as running stratified signal detection and running the report still work.
-   Accept the PR if it solves the problem outlined in the feature request issue, adheres to the coding guidelines and is up to date on the current main HEAD.
-   If any tests fail, reject the PR
-   Use github's PR review functionality and provide constructive feedback

### Coding Guidelines {#coding-guidelines}

-   Follow the [tidyverse style guide](https://style.tidyverse.org/) to maintain code consistency. The RStudio addin [styler](https://styler.r-lib.org/) can help you with that.
-   As an enhancement to the tidyverse style guide, we will make explicit use of namespaces in order to avoid conflicts and improve clarity (i.e. write dplyr::filter() and stats::filter())
-   We stick to this pipe operator %\>%
-   Write clear and descriptive code comments
-   For documentation use [roxygen comments](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html).
-   Keep dependencies updated and minimize external dependencies where possible.
-   For repository structure use [devtools package skeleton](https://bookdown.org/rdpeng/RProgDA/the-devtools-package.html)
-   Run an R-CMD Check using devtools::check() locally before submitting your PR. This helps you already to identify and fix problems. An R-CMD Check will also be initiated automatically after your PR submission as this is one of our Github Actions.
-   The code should be compatible/runnable with R-version 4.0.2

### Commit Message Guidelines {#commit-message-guidelines}

-   Use meaningful commit messages to convey the purpose of your changes.
-   If longer descriptions are necessary, separate the subject from the body with a blank line for clarity.
-   Reference related issues or pull requests where necessary.

### Branching Strategy {#branching-strategy}

-   Create feature branches for new features or enhancements and bug branches for fixing issues.
-   Branch your feature branches off from `develop` as this contains most recent developments.`main` is the branch used for releases and should always stay stable.
-   Branch names should be descriptive and follow the format: `feature/your-feature-name`, `bug/issue-number`.
-   Ideally, finished work from new branches should be reviewed before merging (see pull requests), even if changes were made by a maintainer or owner. If there are only minor changes, you may merge without a review. Use common sense.

### Documentation {#documentation}

-   Maintain and update the project's [documentation](docs/) as needed.
-   Document new features, changes, and usage instructions.

### Testing {#testing}

-   Write unit tests and integration tests for new features and bug fixes.
-   Ensure all existing tests pass before submitting a pull request.
-   Include testing instructions in the pull request if necessary.
-   Use the [testthat](https://testthat.r-lib.org/) package for writing tests

Thank you for contributing to the United4Surveillance signal detection tool!
