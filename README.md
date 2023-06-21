[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit&logoColor=white)](https://github.com/pre-commit/pre-commit)

| **`Windows`** | **`Linux(x86)`** |
|:-----------------:|:-----------------:|
[![Build status](https://ci.appveyor.com/api/projects/status/j9q4qmq166t0svwl/branch/develop?svg=true)](https://ci.appveyor.com/project/rokoDev/cargo/branch/develop)|[![CircleCI](https://dl.circleci.com/status-badge/img/gh/rokoDev/cargo/tree/develop.svg?style=shield)](https://dl.circleci.com/status-badge/redirect/gh/rokoDev/cargo/tree/develop)|

# cargo

## Prerequisites
Install `pre-commit` package. For instructions see: [pre-commit installation](https://pre-commit.com/#install)

In order to be able to make commits with proper formatting and other checks you should run this commands after clonning the repo:
  1. `cd cargo`
  2. `pre-commit install`
  3. `pre-commit install --hook-type prepare-commit-msg`
