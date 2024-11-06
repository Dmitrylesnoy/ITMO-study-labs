javac -d build/classes -sourcepath src -cp libs/Pokemon.jar src/Main.java
jar -vcfm Lab2.jar build/meta/MANIFEST.MF -C build/classes .
java -jar Lab2.jar