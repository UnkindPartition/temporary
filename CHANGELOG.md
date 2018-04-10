## 1.3

* Generated directory names are now based on random hex strings rather than PIDs.

    This got a major version bump as a courtesy to users who may depend on the
    specific form of generated names, but that form is not part of the API
    contract and should not be depended upon.

## 1.2.1.1

* Improve the docs

## 1.2.1

* Limit support to GHC 7.0+
* Add new functions: `writeTempFile,` `writeSystemTempFile,` `emptyTempFile,` `emptySystemTempFile`
* Make sure that system* functions return canonicalized paths
* Modernize the code base, add tests and documentation

## 1.2.0.4

* Update maintainership information
* Fix the docs
