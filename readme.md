##Configure Git for the first time
    git config --global user.name "adrian.wood"
    git config --global user.email "adrian.wood@metoffice.gov.uk"
##Working with your repository
###I just want to clone this repository
If you want to simply clone this repository then run this command in your terminal.

    git clone ssh://git@exxgitrepo:7999/MOOD/metdb-misc.git

###My code is ready to be pushed
If you already have code ready to be pushed to this repository then run this in your terminal.

    cd existing-project
    git init
    git add --all
    git commit -m "Initial Commit" 
    git remote add origin ssh://git@exxgitrepo:7999/MOOD/metdb-misc.git
    git checkout -b develop 
    git fetch
    git merge origin/develop
    git push origin develop

###My code is already tracked by Git
If your code is already tracked by Git then set this repository as your "origin" to push to.

    cd existing-project
    git remote set-url origin ssh://git@exxgitrepo:7999/MOOD/metdb-misc.git
    git push origin develop

**Please note that only the CM Team can commit code directly to the master branch**.
