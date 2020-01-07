isSnapshot := false

version in ThisBuild := "0.4"+(if(isSnapshot.value) "-SNAPSHOT" else "")
