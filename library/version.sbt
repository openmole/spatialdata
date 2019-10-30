isSnapshot := true

version in ThisBuild := "0.3"+(if(isSnapshot.value) "-SNAPSHOT" else "")
