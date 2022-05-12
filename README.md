# An interpreter for the RFun programming language.

RFun was suggested by Yokoyama, Bock and Glück in the article `Towards a
functional reversible language` which was published at RCPL in 2011.
This repo implements the language, with only few changes:

1. The paper suggests a semantics in which linearity analysis i embeded in the evaluation
judgement for expression. Here, we perform the analysis as a separate step, and then we just
assume linearity everywhere else.
2. The paper suggests a reversible semantics, but here we implement two mutually recursive semantics `interpret` and `uninterpret`.

There are several things to be learnt about functional reversible programming languauges,
and but I will not publish the results here, until they are already published in a paper,
for reasons I am sure you will understand.

# Status

[![main-ci](https://github.com/jtkristensen/rcpl/actions/workflows/main.yaml/badge.svg)](https://github.com/jtkristensen/rcpl/actions/workflows/main.yaml)
