# Tasks, Edit Mode and Evaluate Mode

## What is a Task?
A Task has all of the following:

0. A unique ID (Ex. 0)
1. A name (Ex. Violate Antitrust Laws)
2. An optional description (Ex. Monopolize Gilenor's Baking Industry)
3. A status (Ex. Ready)
4. Evaluations from each user, each containing:
    1. An interest ranking from 0 to 4
    2. A priority ranking from 0 to 4
    3. Whether the task needs refinement or not (1 for yes, 0 for no)
A complete overview of each Task can be obtained by using the `cat` command on the task's ID.

## What are the Statuses?
### Blocked (b)
Means that the task is currently inaccessible - often becuase there is another task that it depends on that is undone, or some other reason. These tasks are only visible by using the `ls -a` or `ls -a blocked` command.

### Ready (r)
Means that the task is ready to go - anyone can start working on it! These tasks show up with `ls` and include a number roughly representing which task you should prioritize (based on task evaluations users have done).

### Assigned (a)
Means that the task is currently being worked on by a user (which could include yourself). When `ls`-ing, instead of the task piority number, it shows which user is currently assigned to the task.

### Done (d)
Task is complete, woo! Only visible with `ls -a` or `ls -a done` to avoid clutter. 

## What is Evaluation Mode?
Upon entering the `e` command, the user will enter evaluation mode, allowing the user to rank the tasks. 

### \<interest\> \<priority\> \<needs-refinement\>
In some order, each of the tasks that the user has partially unranked will appear. The user then should input 3 numbers, seperated with spaces, which represent the interest ranking, the piority ranking, and whether the tasks need refinement, in that order.
If less than 3 inputs are given, those values that are not given inputs remain unset.
```C
4 2 0   // ok
3     0          1   // sure?
2 // priority and interest remain unassigned.
5 5 5 // out of bound input, causes an error.
```

### s | skip
Skips evaluation of the current task, and move on to the next one.
### ? | help
Displays a list of all available commands, if you ever need it.
### q | quit
Exit Evaluation Mode, and return to the main REPL.

## What is Edit Mode?
Upon entering the `ed` command with an ID, the user will enter Edit Mode, allowing the user to edit details of the task.

### cat | show
Displays the current task's attributes.
### sd | set-desc
Opens up the user's editor of choice (specified by the EDITOR environment variable set during installation). The user can then input whatever description they want there, and upon saving and exiting the editor it will write it to the description.
### n | rename
Takes one input, which is a string of text in quotations. Use it to change the task's name.
### b | block
Set the task's status to blocked.
### r | ready
Set the task's status to ready.
### a | assign
Takes a username as an input. Sets the task's status to assigned, and assigned it the the user with the given username.
By default, if no username is specified, the task is assigned to the current user.
### d | done
Set a task's status to done.
### e | eval
Edit the evaluation given to the task. Takes 3 three inputs, in the same format and order as in Evaluation Mode.
### ? | help
Displays a list of all the available commands, if you ever need it.
### q | quit
Exit Edit Mode and return to the main REPL.
