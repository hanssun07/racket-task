# Domains and Users

Being able to work to track individual progress is cool and all, but what if you want to share your progress with friends or coworkers? Or maintain some form of work-life seperation and organize tasks seperately? This is where domains and users come in.

> A **domain** is a grouping of tasks that make sense together in some way. For example, you could have a domain to track progress for 1 project, and another domain for another project.

> **Users** are people authorized (by name) to access a certain domain. You can change who you login as for each domain by changing the `(login "XXX")` in the respective domain setting in task.config - but more on this later.

Say you are Elvarg, and you want to get together with your friends Vorkath and Galvek to open a cute bakery together and want to keep track of tasks. However, you and Galvek also want to secretly use the bakery as a front for world domination (Vorkath is unaware of this).

You have also already created two GitHub repositories to allow for the tasks to be shared, and copied down the clone link. However, they are both empty.

## Setting Up Domains and Users
1. Elvarg needs to create two folders on her computer, each of which containing a copy of `empty-domain.dat`. Name it `data.dat`. Elvarg would also initialize these as git repositories.
    > You could also just create the .dat file, if you want. Here's what's in there to start:
    >  ```
    >  ()
    >  (("admin"))
    >  ```
   Here is what the file structure could look like:
   ```
   |- bakery-tasks
   |   \- bakery-data.dat
   |- dom-tasks
       \- domination-data.dat
   |- task
   |- task-sync
   |- task-update
   |- task.config
   ```
2. Elvarg needs to tell Task about these 2 domains:
    1. The `task` script needs to know to check for these 2 folders. At the end of the `task` script, Elvarg should add 2 routes from wherever the `task` script is to the respective folders:
        ```
        check ./bakery-tasks
        check ./dom-tasks
       ```
    2. The `task.config` needs to be able to login to these domains properly. Elvarg should add these to `task.config`:
        ```
        (domain Frontend:
          (datafile "./bakery-tasks/bakery-data.dat")
          (login "admin"))
        (domain EvilPlans:
          (datafile "./dom-tasks/domination-data.dat")
          (login "admin"))
        ```
        The name ___ in `(domain ___:` tells Task what the domain's nickname is, while the datafile field of courses tells where the datafile is located.  
        The login field tells Task which user is logged into this domain - we didn't add any users yet, so we have to use "admin".
3. Elvarg can then login to Task and add Vorkath, Galvek and herself as users:
    ```C
    task // to get into Task
    cd Frontend // swap to the Bakery domain
    u Elvarg // command to add user
    u Galvek
    u Vorkath
    cd EvilPlans // and for world domination...
    u Elvarg
    u Galvek
    q // quit out of task
    ```
4. [OPTIONAL] Elvarg can change the login in `task.config` from `"admin"` to `"Elvarg"` now, no longer needing to log in as admin.
5. Elvarg needs to edit `task-sync` so keeping track of progress with GitHub is easier. To the bottom of the script, add:
    ```
    run ./bakery-tasks <Bakery-repository-link> -b main
    run ./dom-tasks <WorldDom-repository-link>
    ```
    THE `[-b <branchName>]` part is optional, for if you want your changes to go to a specific branch.
6. Finally, Elvarg runs `task-sync` to upload the changes to the shared repository.

## Using a Remote Task Domain
Galvek and Vorkath can now each see the task domain on GitHub, and start tracking their progress. Let's focus on Galvek - for Vorkath it is similar. 
1. Galvek creates 2 folders in his computer to store the domains. Here is what the file structure could look like:
   ```
   |- baking-tasks
   |- destroying-tasks
   |- task
   |- task-sync
   |- task-update
   |- task.config
   ```
2. Galvek edits `task-sync` to track the GitHub repositories that Elvarg set up. He adds:
    ```
    run ./baking-tasks <Bakery-repository-link> -b main
    run ./destroying-tasks <WorldDom-repository-link>
    ```
3. Galvek can now run `task-sync` - if all is correct, the `.dat` files should appear in the respective domains.
4. Galvek needs to similarly edit `task` by adding these 2 to the end:
    ```
    check ./baking-tasks
    check ./destroying-tasks
    ```
5. Galvek also edits `task.config`. Since Elvarg set it up, Galvek can now log in as himself (or admin, if he wants). He adds:
    ```
    (domain Cookings:
      (datafile "./baking-tasks/bakery-data.dat")
      (login "Galvek"))
    (domain Crimes:
      (datafile "./destroying-tasks/domination-data.dat")
      (login "Galvek"))
    ```
6. Galvek can now run `task`, make changes, and when done run `task-sync` to upload the changes onto GitHub for Elvarg and Vorkath to see.

## Summary
- Domains are specially-built `.dat` files that can be shared across systems through repositories like GitHub.
- New users past "admin" are added using the `u <user>` command in Task
- There are 3 places to edit when adding a new domain: `task`, `task.config` and `task-setup` (if nessecary)