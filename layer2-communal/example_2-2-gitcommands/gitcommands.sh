# basic account configuration
git config --global user.name "Jonas Schöley"
git config --global user.email "j.schoeley@gmail.com"
git config --global core.editor "nano -r 72"

# initialize repository
git init

# view remote repositories
git remote -v
# add remote repository
git remote add <remote name> <remote url or ssh>
# remove remote repository
git remote rm <remote name>

# what is the status of the repository
git status

# stage all changed files
git add .
# destage all staged files
git reset HEAD -- .

# what changed in the staged files?
git diff --cached

# commit all staged files
git commit
# commit all staged files with specified message
git commit -m "<message>"

# tagging a commit
git tag <tag>
# with message
git tag <tag> -m "<tag message>"
# tag a specific commit
git tag <tag> <commit checksum>
# show all tags
git tag

# push changes to remote repository
git push -u <remote name> master
# including tags
git push -u --tags <remote name> master
# after histories diverged, e.g. after retrospective migration to git LFS,
# force origin to accept local history
# https://github.com/git-lfs/git-lfs/issues/2991#issuecomment-386757208
git push --force --all origin

# list all branches
git branch --list
# generate a new branch
git branch <new branch name>
# switch to new branch
git checkout <new branch name>

# merge new into old branch
git checkout <old branch name>
git merge <new branch name>

# untrack file
# (needed when adding files to .gitignore which are already tracked)
git rm --cached <filename>

# temporarily save state of project
git stash
# view stashed files
git stash list
# recover last stash
git stash pop

# revert to a previous commit and forget everything that happened since
git reset --hard <commit hash>

# revert to a previous commit without forgetting everything in between
git revert --no-commit <commit hash>..HEAD
git add .
git commit -m "<message>"
