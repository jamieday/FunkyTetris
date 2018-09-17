#!/bin/bash
DEPLOY_DIR=${1:-../jamieday.ca/public/tetris}
APP_VERSION="$(git rev-parse HEAD)"

pushd src
    dotnet fable yarn-build
popd
cp -r public/* "$DEPLOY_DIR"
pushd "$DEPLOY_DIR"
    INITIAL_HEAD=$(git rev-parse HEAD)
    git stash
    STASH_APPLIED=$?
    git add . && git commit -m "deploy tetris version $APP_VERSION"
    clear
    echo | git show --name-only
    echo
    echo "You good with pushing this? (y/N)"
    read CONT
    if [ "$CONT" != "y" ]; then
        git reset --hard "$INITIAL_HEAD"
        if [ $STASH_APPLIED ]; then git stash pop >/dev/null; fi
        exit 0
    fi
    git push
    if [ $STASH_APPLIED ]; then git stash pop >/dev/null; fi
popd