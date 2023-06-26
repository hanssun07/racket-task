# Task

A command-line incremental organization and coordination utility for task management,
powered by Racket.

In alpha. Expect rough and sharp edges and breaking changes.

## Installation and Setup

- Install some kind of terminal.

  On MacOS, use the terminal app.\
  On Windows, [Windows Subsystem for Linux](https://learn.microsoft.com/en-us/windows/wsl/install)
  is recommended.\
- [Install Racket 8.8+](https://download.racket-lang.org/).
  
  Prior versions may work, but 8.8+ is confirmed to work.
- Clone this repository and switch to the `release/alpha` branch.
- Run `defaults/setup.sh` from the base directory to
  compile the code for faster startup,
  set up the default config file, set up a git repository with an empty datafile,
  and set up a run script that synchronizes the git repository and opens up the program.

You may also wish to put all the source code into its own folder, leaving just
`task.config` and `run.sh` in the root directory, modifying them as such.
This makes grabbing updates easier: just run `git pull` and then
recompile with `raco make <src>/main.rkt`.

## Usage and Integration

Without using the default run script, run with `racket main.rkt`.
The program is a command-line interface; `?` or `help` will show available commands.

The data format was chosen so it works well with git, as long as all merges are done through rebases.

## Configuration

You can interoperate among multiple organizations ("domains") by setting things up in the
`task.config` file as such. For instance, if you have your own work, some planning you need
to do within your family, and some coordination you need to do with your hobby group, you can
set up the folder stucture as follows:

```
.
|- self
|  \- data.dat
|- family
|  \- data.dat
\- hobby
   \- data.dat
```

and configure `task.config` with

```
(domain :
    (datafile "self/data.dat")
    (login "me"))
(domain family:
    (datafile "family/data.dat")
    (login "<me>"))
(domain hobby:
    (datafile "hobby/data.dat")
    (login "<me>"))
```

When you start the program you'll be logged in to all three domains.

Furthermore, with all three domains in independent directories, they can be attached to
separate git repositories shared with your respective groups.

## Contributing

This repository has a sister repository [racket-task-tracker](https://github.com/hanssun07/racket-task-tracker)
which provides the datafile in this domain.

All contributions to ideas, designs, bugfixes, minor UI improvements, and documentation are greatly appreciated; for the former two please open an issue, and for the latter three feel free to open a pull request.

For writing features, please feel free to open a pull request with an MVP.
New features need to be carefully integrated into the codebase, as there are
still core features that need to be implemented.
