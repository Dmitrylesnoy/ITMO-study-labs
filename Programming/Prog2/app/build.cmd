javac -d build/classes -sourcepath src -cp libs/Pokemon.jar src/lab/programming/pokemons/Main.java
jar -vcfm Lab2.jar build/meta/MANIFEST.MF -C build/classes .
java -jar Lab2.jar