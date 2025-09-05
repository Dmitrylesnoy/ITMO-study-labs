cd lab0

touch cherrim4
echo 'Возможности Overland=6 Surface=4 Jump=4 Power=1
Intelligence=4 Bloom=0' > cherrim4

touch drilbur6
echo 'Возможности Overland=4 Surface=2
Burrow=6 Jump=2 Power=2 Intelligence=4' > drilbur6

mkdir gengar6
cd gengar6
touch rhyperior
echo 'Живет
Mountaain' > rhyperior
mkdir skiploom
mkdir gloom
touch shelgon
echo 'satk=6 sdef=5 spd=5' > shelgon
touch togepi
echo 'Живет Forest' > togepi
touch masquerain
echo 'Тип покемона BUG FLYING' > masquerain

cd ..
mkdir herdier5
cd herdier5
touch oddish
echo 'Тип диеты
Phototroph' > oddish
touch gastly
echo 'Возможности Sky=8 Power=1 Intelligence=3
Invisibility=0 Phasing=0' > gastly
touch ledyba
echo 'Тип диеты Herbivore' > ledyba
touch roggenrola
echo 'Тип
диеты Nerravore' > roggenrola

cd ..
touch shedinja1
echo 'Способности Leech Life Sand-Attach Fury
Swipes Mind Reader Spite Confuse Ray Shadow Sneak Grudge Heal Block
Shadow Ball' > shedinja1

mkdir watchog3
cd watchog3
mkdir gorebyss
mkdir purugly
touch litwick
echo 'Способности Astonish Ember Minimize Smog Fire
Spin Confuse ray Might Shade Will-0-Wils Flame Burts Impression Hex
Memento Inferno Curse Shadow Ball Pain Split Overheat' > litwick
touch mandibuzz
echo 'Живет
Desert Mountain' > mandibuzz
mkdir munna
mkdir fearow
cd ..


chmod 400 cherrim4
chmod u=rw,g=r,o-rwx drilbur6

cd gengar6
chmod =-w-r-x,g=rw rhyperior
chmod =x,u+r,g+rw,o+w skiploom
chmod uo+w,g+wr gloom
chmod =-x,ug=w,u+r shelgon
chmod u=rw,g=w,o=-r-w-x togepi
chmod 644 masquerain
cd ..
chmod 755 gengar6

cd ./herdier5
chmod =-x,ug+w,u+r,o-r oddish
chmod 046 gastly
chmod -rwx,u+r ledyba
chmod u=rw,g=w,o=w roggenrola
cd ..
chmod 571 herdier5
chmod 404 shedinja1
cd watchog3
chmod ugo=rwx,g-r gorebyss
chmod ugo=w,uo+x,g+r purugly
chmod -rwx,o+r litwick
chmod ug=,o=+rw mandibuzz
chmod uo=wx,g=rw munna
chmod ug=rwx,g-w,o=g-x fearow
cd ..
chmod u=rx,g=w,o=r watchog3

chmod u+rwx ./watchog3/litwick
chmod u+rwx ./watchog3/litwick
chmod 004 ./watchog3/litwick
ln ./drilbur6 ./gengar6/togepidrilbur
cp ./drilbur6 ./watchog3/fearow
ln -s ./gengar6 ./Copy_97
ln -s ./shedinja1 ./gengar6/rhyperiorshedinja
cat shedinja1 > ./watchog3/litwickshedinja
chmod -R u=rx ./watchog3
chmod -R u=rwx ./gengar6/gloom
cp -R ./watchog3 ./gengar6/gloom
cd ./gengar6/gloom/watchog3
chmod ugo=rwx,g-r gorebyss
chmod ugo=w,uo+x,g+r purugly
chmod -rwx,o+r litwick
chmod ug=,o=+rw mandibuzz
chmod uo=wx,g=rw munna
chmod ug=rwx,g-w,o=g-x fearow
cd ..
chmod u=rx,g=w,o=r watchog3
cd ..
chmod uo+w,g+wr gloom
cd ../watchog3/
chmod ugo=rwx,g-r gorebyss
chmod ugo=w,uo+x,g+r purugly
chmod -rwx,o+r litwick
chmod ug=,o=+rw mandibuzz
chmod uo=wx,g=rw munna
chmod ug=rwx,g-w,o=g-x fearow
cd ..
chmod u=rx,g=w,o=r watchog3

wc -m cherrim4 | cat >> cherrim4
ls -Rtl | grep -R 'she' .
grep -Rnv 'He' ./herdier5
cat -n drilbur6 2>>//tmp/err_s466513 | sort
cat -n ledyba roggenrola | grep -iv 'e$'
ls -Rl | grep "ro" | sort | tail -n 3

chmod u+w shedinja1
rm shedinja1
rm ./gengar6/masquerain
rm ./gengar6/rhyperiorshedin*
rm ./gengar6/togepidrilb*
chmod -R u+rw watchog3
rm -R watchog3
rmdir watchog3/gorebyss