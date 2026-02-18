# Contributing

If you discover issues, have ideas for improvements or new features,
please report them to the [issue tracker][1] of the repository or
submit a pull request. Please try to follow these guidelines when you
do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `main`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Mention your Emacs version and operating system.
* Mention the `copilot.el` version (or commit hash) and the Node.js version you
  are using.
* Include any relevant code or `*Messages*` output.

## Pull requests

* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][2].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Make balancer syntax-aware`).
* Update the [changelog][3].
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (<kbd>C-c ? d</kbd>).
* Squash related commits together.
* Open a [pull request][4] that relates to *only* one subject with a clear title
  and description in grammatically correct, complete sentences.
* Include tests when adding new functionality or fixing bugs. We use
  [Buttercup][5] for testing.
* CI runs the test suite and linters automatically on every pull request.

## Development setup

1. Fork and clone the repository.
2. Install [Eask][6].
3. Run `eask install-deps --dev` to install dependencies.
4. Run `eask compile` to byte-compile.
5. Run `eask test buttercup` to run the test suite.
6. Run `eask lint checkdoc` to check documentation conventions.

**Note:** macOS users should make sure that the `emacs` command resolves the
version of Emacs they've installed manually (e.g. via `homebrew`), instead of
the ancient Emacs 22 that comes bundled with macOS. See [this article][7] for
more details.

[1]: https://github.com/copilot-emacs/copilot.el/issues
[2]: https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[3]: https://github.com/copilot-emacs/copilot.el/blob/main/CHANGELOG.md
[4]: https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests
[5]: https://github.com/jorgenschaefer/emacs-buttercup
[6]: https://emacs-eask.github.io/
[7]: https://emacsredux.com/blog/2015/05/09/emacs-on-os-x/
