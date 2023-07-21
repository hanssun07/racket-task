# The Task REPL

After you have logged onto Task by typing `task`, you should be greeted with a list of all the task domains you're logged into, as well as a prompt for commands.

Continuing our example from earlier, Vorkath has set up his domains and has logged in. By typing in the command `ld`, he sees 2 domains: `:`, the root domain; and `bakery` (assuming he named the domain "bakery" in `task.config`).

When Task initializes, it always starts in the root domain. From here, Vorkath can type in any of the valid commands to do things. Commands have a full name and a shorthand.

### ? | help

Pretty self explanatory - if you ever get lost or forget the name of a command, this summons the list of all valid commands.

### : | summary

This command provides a summary of tasks that have been assigned to the users logged in, tasks that are pending assignment, as well as a count for how many tasks need evaluation by the user. This takes into account tasks from _all domains_.

## Domain Management

### ld | list-domains
Displays a list of all currently available task domains.

### cd | change-domain
`change-domain` allows the user to hop between different task domains. It takes one input as well - the path to the other domain. Shortcuts also exist to hop from the root domain or to the home domain:

```C
    cd /...    // from the root domain
    cd ~       // to the home domain
    cd         // to the home domain
```

For example, Vorkath can switch to the `bakery` domain with `cd bakery`.

### \<path\>:\<command\>
If you want to run a command in a specific domain without first `cd`-ing into it, this is the command for you. Simply prepend any other command (along with it's inputs) with the path and a colon `:`.
For example, if Vorkath were in the root `:` domain, he could run `bakery:cat 0` to run `cat 0` within the bakery task domain.

## User Management

### u | new-user
Adds a new user to the current task domain. Takes one input, which is the name of the user, no quotations nessecary.
For example, if Vorkath wants to add Bob to help on the tasks, he could do `u Bob`. After saving these changes, Bob can now setup `task.config` to login as "Bob".

### swap-user
This command lets you login to a different user. Upon typing in the command, a prompt will appear. Type in a valid username to swap to that user.

## Task Management

### n | new-task

`new-task` creates a new task in the domain you are currently in. It takes one input, the name of the task in quotations.
For instance, after Vorkath switches to the `bakery` domain, he could do:

```C
    n "Perfect recipe for bread"
    n "Get Food Safety Approval for dragonfire cooking"
    n "Finish budgeting for Quarter 4"
    n "Violate antitrust laws"
```

to initialize 4 new tasks.

### ls | list

Lists out all tasks in the current task domain, including their ID, name, status, and either priority score or current assignee. `ls` can be augmented with options to filter results:

- `-a` will display all tasks, even tasks that `ls` usually hides for brevity
- `-a [word]` displays all tasks that meet a certain criteria, dependent on the word. - `ready` / `assigned` / `done` will only show tasks that have the "ready", "assigned", or "done" status respectively - `blocked` / `unassigned` / `not-done` are the inverses of the above, showing all tasks without the matching status - `mine` shows all tasks assigned to the current user - a `<number>` can be used to indicate a maximum number of tasks to show - `-t` or `by-id` will list tasks in ascending id order - `-p` or `by-priority` will list tasks in descending priority score
- Options can also be used together to achieve precision filtering - in this case, the options are applied from left to right. For instance, `ls -a mine -p 3` will display the 3 most prioritized tasks assigned to the current user.

### cat | view-task
Given the id of a task (for instance, `cat 0`), it will provide more details about the task in the current task domain, such as a detailed description, the priority score, and timestamps for when the task was created, finished etc.

### e | eval
Enters evaluation mode, allowing the user to assign priority, interest and whether the task description needs to be refined.

### ed | edit
Given a Task ID (for instance, `ed 0`), enters edit mode, allowing the user to edit the task description and name, as well as assign the task or otherwise change the task's status.

## Saving and Exiting

### reload
Unknown Behavior? It seems to run an infinite loop for now...

### commit
Unknown Behavior? It seems to do nothing for now...

### q | quit
Save all your changes, commiting all changes in all task domains. It then quits out of Task.

### q! | exit
Ragequit out of Task, discarding all changes from the current session.
