# Task Setup as CLI Commands

Copy the contents of this directory into some reasonable place, say
`~/task` or `/usr/local/task`, and add that directory to your path.

The scripts are
- `task` which launches the task program,
- `task-sync` which sets up/synchronizes all domains with their
  upstream git repositories, and
- `task-update` which installs/updates to the task program.
  `task-update <branch>` switches over to a particular branch;
  `release/alpha` has the rough edges, `main` has the sharp edges,
  and if you're a contributor you can use a personal dev branch to
  test your changes.

Revise the following files to suit your needs:
- `task-sync`, with the actual folders and repositories you intend to use.
  Revise the calls at the bottom to `run()`: the arguments are
  ```
  run <dir> <repo-uri> [-b <branchname>]
  ```
- `task`, which checks each domain to remind you if you need to sync.
  Revise the calls at the bottom to `check()`: the argument is the
  directory.
- `task.config`, with the relevant config details.
