# sonar-bulk-analyzer

Tool for bulk project analysis in SonarQube

This tool uses the following pipeline:
- Parse the input file with projects
- Clone the projects
- Build the projects
- Create the projects in SonarQube
- Set default profile to the profile you specified
- If project uses Gradle inject the sonar plugin into the build.gradle file.
- Run the analysis

# Usage

```
$ java -jar sonar-bulk-analyzer-0.1.jar --help
Sonar Bulk Analyzer 0.1-beta
Usage: sba [build] [options]

  -o, --out <dir>          Directory where projects will be cloned to
  -r, --repoFile <file>    File where to take repositories to clone from
  -p, --parser <parserClass>
                           Parser to use when parsing repository file
  -e, --error <dir>        Directory to store errors and output from build proccess
  -s, --sonar-version <version>
                           SonarQube plugin version to add to Gradle files
  -t, --token <token>      Token to use when authenticating with SonarQube
  --sonar-url <url>        SonarQube location
  --default-profile <profile>
                           Default profile to set for Sonar projects
Command: build
Build the projects in provided directory
  --help                   Display help
```


# Issues

- Gradle build process too custom
  - How to include plugins node into the gradle build file 
- External dependencies such as need to add some kind of files before build
- Android SDK required to build Android projects
- SDK license manager
- Some projects are missing build systems, so its not possible to programmatically compile them efficiently
