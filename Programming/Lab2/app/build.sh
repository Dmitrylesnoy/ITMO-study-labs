javac -d build/classes -cp libs/Pokemon.jar src/Main.java
cd build
jar -v -c -f Lab2.jar --manifest=./meta/MANIFEST.MF -C classes .




