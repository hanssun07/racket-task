# Installing Task

This installation guide was written on July 12th, 2023. Since Task is still in alpha, the installation method may change in the future.

1. Clone the contents of the `setup/as-command` branch into a repository of your choosing.  
    ```
    git init
    git clone -b setup/as-command https://github.com/hanssun07/racket-task.git
    ``` 
    > **Check:** Along wih the README, you should see `empty-domain.dat`, `task-config`, and 3 scripts: `task`, `task-sync`, `task-update`.
2. Update your system PATH variable to include the directory above. The method to do this varies between Operating System, so look it up if you must.
    > **Check:** *Restart the Terminal First!* If you can type `task` into the terminal and *NOT* get "command not found", you did this step right (It should still error out though)
3. In your terminal's configuration, update your terminal's variables to export the EDITOR variable. This variable tells Task what text editor to boot up to make changes to tasks. For example, in a Bash Shell, append to the `.bashrc` file:
    ```
    export EDITOR=vim
    ```
    to make Task boot up vim.
    > This might vary depending on choice of terminal - if in doubt, look it up!
4. Run this in the terminal to grab the most recent version of Task:
    ```
    task-update
    ```
    Optionally, specify which branch to grab by instead using `task-update <branch>`. 
    > `task-update` is also how you update the Task program, so run it occasionally! 

    > **Check:** You should see a folder called `program` be created in the repository containing the scripts.

That's all the configuration on the system - now, we need to clear the scripts and reboot Task's configuration.
> **Note:** If you wish to add multiple task domains or manage user login behaviour, you would be changing these files! 

5. `task-sync` is for if you wish to share your tasks with other people through the internet, like with a GitHub repository! To explain fully we would need to discuss what a task domain is, which is done later.
    Go ahead and just remove the 3 sample `run` commands listed at the end, or commment them out for now.
6. `task` is the main application, and needs to know where to look to save data. Again, has to do with domain stuff. 
    For now, just remove the 3 sample `check` commands, or comment them out.
7. `task.config` tells task about each of the task domains - what they are, where to find them, user info etc.
    For now, remove code so that it only reads:
    ```
    (domain :
    (datafile "empty-domain.dat")
    (login "admin"))
    ```
8. Good to go! Run `task`, and you eventually be greeted with something like:
    ```
    Logged in as :admin
    admin@:>
    ```
   And you're in the REPL!