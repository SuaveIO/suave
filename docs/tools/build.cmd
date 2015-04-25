
fsi generate.fsx

git clone http://github.com/SuaveIO/suave clone -b gh-pages

cd clone

del *
rmdir /s reference
xcopy /s /y ../../output/* .

git add .
git commit -a -m "updates to docs"
