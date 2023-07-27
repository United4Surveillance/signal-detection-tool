# Questions

## Styleguide

- Should we simply use the tidyverse style guide https://style.tidyverse.org/ ?

It is probably the most common way.

## Target architecture

- Do we want the final product to be an R-package, maybe even submit it to CRAN?

Should be the most reproducible but may be more work during the development process.

- Or do we want to provide a code repository that can be clowned and then used? 

Reproducability could be achieved by using renv and maybe a fixed (portable) R-version.

In this way it is easy to set up at any organization and should be stable. However using renv might not correspond perfectly to the open source development philosophy.

## latex issues

- How can we make sure that latex and its packages are installed correctl on the target systems,this often leads to problems?

Maybe tinytex can be the solution, it can be installed via R.
