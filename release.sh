#!/bin/bash
DATE=`date +%Y-%m-%d`
BASE=sms-bin

cd frontend
sencha app build production
cd ..
pushd ~/$BASE
git branch $DATE
git checkout $DATE
popd

mkdir -p ~/$BASE/backend/scripts ~/$BASE/frontend
cp ~/.local/bin/backend ~/$BASE/backend
cp -r backend/scripts/* ~/$BASE/backend/scripts
cp ~/.local/bin/minuterun ~/$BASE/backend
cp -r frontend/build/production/SMS/* ~/$BASE/frontend
pushd ~/$BASE
git add *
git commit -m "$BASE version $DATE"
git push origin $DATE
git checkout master
popd
