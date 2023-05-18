# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- swank-call.el: An wrapper over slime eval to make calls into the
  slime sub-process more lispy.
  
- logterm.el: An ansi-terminal in buffer, but over a fifo, to be used
  as an output device.
  
- cl-test.el: A sketch for the emacs side interface to a common lisp testing framework.

- simulation-cl/, load-simulation.lisp: Simulation (mock-up) of a testing
  framework on the common lisp side.

- cl-test-demo (in cl-test.el): A scripted demonstration of a test
  run, using the interface sketch and the simulation.
