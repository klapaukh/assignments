#Assignment Attempt Explorer

This tool seeks to help people look at what assignment non-submission may say 
about a students subsequent likelihood of passing a course. If you use this
system in research please cite it.

## Invocation

To run the system you need to run the general.R script. This will give the URL
of a locally hosted website which can be opened to see the results. Since this
uses shiny the webpage will only be visible while the script continues to run.

## Command line options

The minimal invocation instruction is

```
./general.sh --input inputFile.csv
```

There are a number of additional arguments that  can also be passed to the
system. Passing `--help` will print the help.

## Expected file format

The input file must be a CSV file, with each row representing a single student.
The first row in the file must be a header row. The following columns are
expected:
   * ID - A unique identifier for each student
   * Final - The students final mark in the course
   * a1.. - A set of columns all with a fixed prefex ('a' by default) to
     indicate a mark for each assignment. Each column has a number in it to
     denote which assignmnet it is 
 
All other columns are ignored (but may be present). 

