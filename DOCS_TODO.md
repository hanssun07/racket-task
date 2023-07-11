### Task

Task is an incremental command-line task manager system, complete with multiple users, task domains, and much more!   
Powered by Racket, and in active alpha. 

### Table of Contents

1. Installation Tutorial   
    1. Installing Task
    2. Configuring Task with the Example Task Domain
2. The General Order   
    1. Domains - the Domain Pipeline, how to set one up - config, sync, task etc.
    2. Users - adding Users past the first
    3. Tasks - Creation, Status, Evaluation
    4. Tasks - Assigning, Editing, Completing, Blocking
    5. Git - restore, commit.
    6. UI - Summary, Help, Exit
3. Formal Documentation   
      Here is where I would list out each function in the repl (both normal and edit mode) and describe what each does. 
      Enough detail to bore, probably... 

### Scattered Writings that can be grouped later

## I - Domain Stuff

After installing Task and configuring the related PATH and EDITOR variables, you are now ready to start configuring the different "environments" that your tracked tasks will live in. These "environments" are called **domains**, and are useful to help seperate concerns apart from each other. You wouldn't want development plans for a cute cat game to get mixed together with your plans for WORLD DOMINATION, would you?

Task domains are simply a `.dat` file with a git repository wrapped around it. To create, and more importantly access, a new task domain:
1. Make a copy of `empty-domain.dat` as the sole item inside of a folder of your choosing.
2. Edit the `task` script. At the very end, append the following:
    ```
    check "<path to folder containing .dat file here>"
    ```
    This lets Task know to save your progress to the correct place.
3. Edit the `task.config` file. Add to the very end:
    ```
    (domain <XXX>:
      (datafile "<path to desired .dat file>")
      (login "admin"))
    ```
    - XXX can be anything you want - this is the "Nickname" that task will give to the domain.
    - Note that the path must end in a `.dat`, instead of just a folder
    - Users for a domain are registered in the domain file itself. Since we used `empty-domain.dat`, the only user currently available is `admin`. We have to configure it like this for now, we can add and change users later!
4. [OPTIONAL] Task also supports sharing domains across GitHub etc. If you wish to do so, add this to the end of `task-sync`:
    ```
    run <path to folder here> <repo-uri> [-b branchname]
    ```
    Running `task-sync` in the terminal before and after sessions will update/upload the domain to the remote repository accordingly.
Now, running `task` should give you access to your shiny new repository, and it wil show up with it's nickname.

## II - The Task System
There are 4 states that a task can be in:

### Blocked (b)

Means that the task is currently inaccessible - often becuase there is another task that it depends on that is undone, or some other reason. These tasks are only visible by using the `ls -a` or `ls -a blocked` command.

### Ready (r)

Means that the task is ready to go - anyone can start working on it! These tasks show up with `ls` and include a number roughly representing which task you should prioritize (based on task evaluations users have done).

### Assigned (a)

Means that the task is currently being worked on by a user (which could include yourself). When `ls`-ing, instead of the task piority number, it shows which user is currently assigned to the task.

### Done (d)

Task is complete, woo! Only visible with `ls -a` or `ls -a done` to avoid clutter. 