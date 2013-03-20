scad-clj
========

OpenSCAD DSL in Clojure

** OpenCV

cmake -DBUILD_SHARED_LIBS=OFF -DWITH_V4L=OFF -DCMAKE_INSTALL_PREFIX=/usr/local/opencv ..

** Maven Repo

mvn deploy:deploy-file -Dfile=/usr/local/opencv/share/OpenCV/java/opencv-244.jar -DartifactId=opencv -Dversion=2.4.4 -DgroupId=opencv -Dpackaging=jar -Durl=file:repo -DcreateChecksum=true
