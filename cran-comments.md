This is the resubmission of the package to CRAN. Previously, a maintainer requested the following changes, which have all been implemented:

1. The redundant "Provides tools to" from the description has been omitted.
2. All the instances of \dontrun{} & \donttest have been removed as the examples are executable, and in < 5 sec.
3. Examples that need packages in 'Suggests' are wrapped with the if statement: if (requireNamespace("pkgname")) {}



The total check time is expected to be below 10min. There are no ERRORs or WARNINGs. There is a possible NOTE due to this being a new submission:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Clement Lee <clement.lee.tm@outlook.com>'

New submission
