# Aidan Brookes - Individual Homework #3 Repository

## Viewing your class repo(s) on GitHub

At the following page, you should see a repo for each assignment, and
an individual/team repo for each assignment, where you can submit
your work. All of these repos are private, so you'll need
to log in to Github first.

[https://github.com/mines-csci400](https://github.com/mines-csci400)

## Git remotes

Git "remotes" allow you to push/pull to/from other repositories.
To look at your current remotes:
```
git remote -v
```
The default remote is called `origin`, and it points to your
repository:
```
https://github.com/mines-csci400/f20b-user-abrookes-hw03.git
```

## Working on an assignment

To bring in a new assignment, you should create a remote that points to the
corresponding assignment:
```
git remote add upstream https://github.com/mines-csci400/f20b-assignment-hw03.git
git pull upstream master --allow-unrelated-histories
```
You can then edit the files as
needed to complete the assignment, make as many commits as needed, and
finally do the following to push to your user repo:
```
git push origin master
```
If for some reason you need to bring in the instructor's version of the
assignment again (e.g., due to bug fixes, hints, clarifications, etc.):
```
git pull upstream master
```
(no need for `--allow-unrelated-histories` the second time).
