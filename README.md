[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/codemetrics.svg)](https://jcs-emacs.github.io/jcs-elpa/#/codemetrics)

# Code Metrics
> Plugin shows complexity information

[![CI](https://github.com/jcs-elpa/codemetrics/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/codemetrics/actions/workflows/test.yml)

This plugin implements live calculation of the **Cognitive Complexity** metric,
which was proposed by G. Ann Campbell in
[Cognitive Complexity - A new way of measuring understandability](https://www.sonarsource.com/docs/CognitiveComplexity.pdf)
(c) SonarSource S.A. 2016-2021, Switzerland.

> **Abstract:** Cyclomatic Complexity was initially formulated as a measurement
> of the "testability and maintainability" of the control flow of a module.
> While it excels at measuring the former, its underlying mathematical model is
> unsatisfactory at producing a value that measures the latter. This white paper
> describes a new metric that breaks from the use of mathematical models to
> evaluate code in order to remedy Cyclomatic Complexity’s shortcomings and
> produce a measurement that more accurately reflects the relative difficulty of
> understanding, and therefore of maintaining methods, classes, and applications.

## Configuration

## 🖥 Usage

These are functions you can use to analyze:

| Functions             | Description                        |
|-----------------------|------------------------------------|
| `codemetrics-analyze` | Analyze a string with `major-mode` |
| `codemetrics-region`  | Analyze region                     |
| `codemetrics-buffer`  | Analyze the whole buffer           |

All these functions returns a score indicates the complexity.

## 🔨 Supported languages
> ⚠️ Please sort these two lists alphabetically!

These languages are fairly complete:

- C / C++ / C#
- Java

These languages are in development:

- Agda
- Elm
- Emacs Lisp
- Go
- JavaScript / JSX / Julia
- Lua
- OCaml
- PHP / Python
- Ruby / Rust
- Scala / Swift
- TypeScript / TSX
- XML (upstream)

## 🔗 References

- [codemetrics](https://github.com/kisstkondoros/codemetrics)
- [resharper-cognitivecomplexity](https://github.com/matkoch/resharper-cognitivecomplexity)
- [gocognit](https://github.com/uudashr/gocognit)

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
